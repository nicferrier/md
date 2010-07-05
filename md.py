#!/usr/bin/python

"""
md - A maildir tool.

md provides three important things:

1. a maildir folder object allowing API access at a low level

2. a maildir client object allowing API access at a high level

3. a command line client API for cmd line use, shell scripting or tool building.

Author: nic ferrier - nic@ferrier.me.uk
"""

import sys
import re
import os
from os.path import exists
from os.path import abspath
from os.path import join as joinpath
from os.path import split as splitpath

import time
from datetime import datetime

from email.parser import FeedParser
from email.parser import HeaderParser
from email.parser import Parser
from email.utils import parsedate_tz
from email.utils import parsedate
from email.utils import parseaddr
from email.utils import mktime_tz

try:
    import json
except ImportError:
    import simplejson as json

import pdb
from StringIO import StringIO
import logging

logger = logging.getLogger("md")
logging.basicConfig()

folderlist = {}

class HeaderOnlyParser(HeaderParser):
    """JUST parse the header

    Python's HeaderParser actually parses the whole message. Duh."""

    def parse(self, fp, headersonly=True):
        """Create a message structure from the data in a file."""
        feedparser = FeedParser(self._class)
        feedparser._set_headersonly()
        strbuf = StringIO()
        #mp = mmap.mmap(fp._file.fileno(), 2048, mmap.PROT_READ)
        for line in fp:
            #print line
            strbuf.write(line)
            if line == "\n":
                break
        data = strbuf.getvalue()
        feedparser.feed(data)
        return feedparser.close()

hdr_parser = HeaderOnlyParser()

from pyproxyfs import Filesystem
OSFILESYSTEM = Filesystem()
MDMSGPATHRE = "%s/(?P<key>[0-9]+\\.[A-Za-z0-9]+)\\.(?P<hostname>[.A-Za-z0-9-]+)(:[2],(?P<flags>[PRSTDF]*))*"
SEPERATOR="#"
class MdMessage(object):
    def __init__(self, 
                 messagefd, 
                 key, 
                 filename="", 
                 folder=None, 
                 filesystem=OSFILESYSTEM):
        try:
            self.content = messagefd.read()
        except AttributeError:
            self.content = ""

        self.msgobj = hdr_parser.parse(
            StringIO(self.content), 
            headersonly=True
            )
        self.headers_only = True
        self.folder = folder
        self.filename = filename
        self.filesystem = filesystem
        self.msgpathre = re.compile(MDMSGPATHRE % joinpath(
                self.folder.base,
                self.folder.folder, 
                "cur"
                ))
        self.key = key
        self.hdrmethodre = re.compile("get_([a-z_]+)")
        try:
            d = parsedate_tz(self.msgobj["Date"])
            t = mktime_tz(d)
            self.time = t
        except Exception, e:
            try:
                self.time = time.mktime(parsedate(self.msgobj["Date"]))
            except Exception, e:
                self.time = -1

    def walk(self):
        if self.headers_only:
            self.msgobj = Parser().parse(StringIO(self.content))
        return self.msgobj.walk()

    def get_content_type(self):
        if self.headers_only:
            self.msgobj = Parser().parse(StringIO(self.content))
        return dict(self.msgobj._headers).get("Content_type", "text/plain")

    def __getattr__(self, attrname):
        """Implements the get methods for SMTP headers on the embedded message."""
        msg = self.__dict__["msgobj"]
        m = self.__dict__["hdrmethodre"].match(attrname)
        if msg and m:
            hdrs = dict(msg.__dict__["_headers"])
            attr = m.group(1)
            fieldname = "".join([attr[0].upper(), attr[1:]])
            return lambda: hdrs.get(fieldname, "")

    def iteritems(self):
        """Present the email headers"""
        for n,v in self.msgobj.__dict__["_headers"]:
            yield n.lower(), v
        return

    def items(self):
        """Present the email headers"""
        return list(self.iteritems())

    def _flags(self):
        m = self.msgpathre.match(self.filename)
        return m.groups("flags")

    def _set_flag(self, flag):
        """Turns the specified flag on"""
        # TODO::: turn the flag off when it's already on
        def replacer(m):
            return "%s/%s.%s%s" % (
                joinpath(self.folder.base, self.folder.folder, "cur"),
                m.group("key"),
                m.group("hostname"),
                ":2,%s" % (
                    "%s%s" % (m.group("flags"), flag) if m.group("flags") \
                        else flag
                    )
                )
        newfilename = self.msgpathre.sub(replacer, self.filename)
        self.filesystem.rename(self.filename, newfilename)
        self.filename = newfilename

    @property
    def date(self):
        return datetime.fromtimestamp(self.time * 1.0)

    @property
    def is_seen(self):
        return "S" in self._flags()

    @is_seen.setter
    def is_seen(self, value):
        self._set_flag("S")

    @property
    def is_trashed(self):
        return "T" in self._flags()

    @is_trashed.setter
    def is_trashed(self, value):
        self._set_flag("T")

    def __repr__(self):
        return "%s__%s__%s" % (
            self.__class__.__name__, 
            self.folder,
            self.key
            )

class MdFolder(object):
    """A Maildir folder.

    Maildir is a lockless mail store.
    """

    def __init__(self, foldername, base="", subfolder=False, filesystem=OSFILESYSTEM):
        self.base = base
        self.folder = foldername
        self.is_subfolder = subfolder
        self.filesystem = filesystem

    def _foldername(self, additionalpath=""):
        return joinpath(self.base, self.folder, additionalpath) \
            if not self.is_subfolder \
            else joinpath(self.base, ".%s" % self.folder, additionalpath)

    def folders(self):
        """Return an object the holds the folders for this folder.

        This is a snapshot of the folder list at the time the call was made. 
        It does not update over time.
        """

        entrys = self.filesystem.listdir(abspath(self._foldername()))
        just_dirs = [d for d in entrys if re.match("\\..*", d)]

        folder = self._foldername()
        filesystem = self.filesystem

        class FolderList(object):
            def __iter__(self):
                return just_dirs.__iter__()

            def __list__(self):
                return list(self.__iter__())

            def __contains__(self, item):
                return just_dirs.__contains__(".%s" % item)

            def __getitem__(self, index):
                return MdFolder(
                    just_dirs[index],
                    base=folder,
                    filesystem=filesystem)

        f = FolderList()
        return f

    def __repr__(self):
        return "<%s>" % (self.folder)

    def _muaprocessnew(self):
        """Moves all 'new' files into cur, correctly flagging"""
        files = self.filesystem.listdir(self._foldername("new"))
        for filename in files:
            if filename == "":
                continue
            newfilename = joinpath(
                self._foldername("cur"),
                "%s:2,%s" % (filename, "")
                )
            self.filesystem.rename(
                self._foldername(joinpath("new", filename)),
                newfilename
                )

    def _curiter(self):
        """An iterator over the messages in 'cur'"""
        for filename in self.filesystem.listdir(self._foldername("cur")):
            yield joinpath(self._foldername("cur"), filename)

    def _exists(self, key):
        """Find a key in a particular section

        Searches through all the files and looks for matches with a regex.
        """
        self._muaprocessnew()
        files = list(self._curiter())
        for filename in files:
            m = re.match(
                # Flags are the initial letter of the following:
                # Passed, Replied, Seen, Trashed, Draft, Flagged
                # Here's a better regex than the one we're using
                # :(?P<version>[2])(?P<flags>[PRSTDF]*))
                "%s/%s\\.([.A-Za-z0-9-]+)(:[2],([PRSTDF]*))*" % (
                    self._foldername("cur"),
                    key
                    ),
                filename
                )
            if m:
                return (
                    filename,
                    m.group(1), # hostname 
                    m.group(3) if m.group(2) else "" # flags
                    )
        raise KeyError("not found")
            
    def __getitem__(self, key):
        try:
            # Cache this stuff against key?
            path, host, flags = self._exists(key)
            with self.filesystem.open(path) as msgfd:
                content = msgfd.read()
                return MdMessage(
                    StringIO(content), 
                    key, 
                    filename=path, 
                    folder=self, 
                    filesystem=self.filesystem
                    ) 
        except KeyError, e:
            e.message = "no such message %s" % key
            raise

    def __iter__(self):
        #pdb.set_trace()
        self._muaprocessnew()
        for filename in self._curiter():
            m = re.match(
                MDMSGPATHRE % (self._foldername("cur")),
                filename
                )
            if m:
                yield m.group("key")
        return

    def iterkeys(self):
        return self.__iter__()

    def iteritems(self):
        for k in self.iterkeys():
            yield k,self[k]

    def keys(self):
        return list(self.iterkeys())

    def values(self):
        return list([self[k] for k in self.iterkeys()])

    def items(self):
        return list(self.iteritems())

def _escape(match_obj):
    if match_obj.group(0) == "\n":
       return ""
    if match_obj.group(0) == "\"":
        return "\\\""
    return

class MdClient(object):
    def __init__(self, maildir, filesystem=None):
        self.logger = logging.getLogger("MdClient.%s" % maildir)
        foldername = maildir.split("/")[-1]
        base = splitpath(maildir)[0]
        self.folder = MdFolder(
            foldername if foldername else "",
            base=base,
            filesystem=filesystem
            ) if filesystem else MdFolder(foldername if foldername else "",
                                          base=base
                                          )

    def _getfolder(self, foldername):
        mf = MdFolder(
            foldername=foldername,
            base=joinpath(self.folder.base, self.folder.folder),
            filesystem=self.folder.filesystem,
            subfolder=True
            )
        return mf

    def _list(self, foldername="INBOX"):
        """Do structured list output.

        Sorts the list by date.
        """
        folder = self.folder \
            if foldername == "INBOX" \
            else self._getfolder(foldername)

        lst = list(folder.iteritems())
        for key,msg in sorted(lst, key=lambda d: d[1].date):
            yield folder, key, msg
        return

    def ls(self, foldername="INBOX", stream=sys.stdout):
        """Do standard text list of the folder to the stream"""
        for folder, mk, m in self._list(foldername):
            try:
                # I am very unsure about this defaulting of foldername
                print >>stream, "% -20s % 20s % 50s  [%s]  %s" % (
                    "%s%s%s" % (folder.folder or foldername or "INBOX", SEPERATOR, mk),
                    m.date,
                    m.get_from()[0:50] if m.get_from() else "", 
                    m.get_flags(),
                    re.sub("\n", "", m.get_subject() or "")
                    )
            except Exception,e:
                self.logger.exception("whoops!")

    def lisp(self, foldername="INBOX", stream=sys.stdout):
        """Do JSON list of the folder to the stream"""
        def fromval(hdr):
            if hdr:
                return parseaddr(hdr)

        for folder, mk, m in self._list(foldername):
            try:
                print >>stream, json.dumps({
                        'folder': folder.folder,
                        'key': "%s%s%s" % (folder.folder, SEPERATOR, mk),
                        'date':  str(m.date),
                        "flags": m.get_flags(),
                        'from': fromval(m.get_from()),
                        'subject': re.sub("\n|\'|\"", _escape, m.get_subject() or "")
                        })
            except Exception, e:
                self.logger.exception("whoops!")

    def lsfolders(self, stream=sys.stdout):
        """List the subfolders"""
        for f in self.folder.folders():
            print >>stream, f

    def _get(self, msgid):
        foldername, msgkey = msgid.split(SEPERATOR)
        folder = self.folder if foldername == "INBOX" else self._getfolder(foldername)
        # Now look up the message
        msg = folder[msgkey]
        msg.is_seen = True
        hdr = msg.items()
        for p in msg.walk():
            yield hdr,p
        return

    def gettext(self, msgid, stream=sys.stdout):
        """Get the first text part we can find and print it as a message.

        This is a simple cowpath, most of the time you want the first plan part.
        """
        for hdr,part in self._get(msgid):
            if part.get_content_type() == "text/plain":
                for hdr,val in hdr:
                    print >>stream, "%s: %s" % (hdr,val)
                print >>stream, part.get_payload(decode=True)
                break

    def getstruct(self, msgid, stream=sys.stdout):
        """Get and print the whole message.
        """
        for hdr,part in self._get(msgid):
            if part.get_content_type() == "text/plain":
                print >>stream, part.get_content_type()


## This should be redefined as an option and a thread local
## Default value should come from an env var or be ~/Maildir
HOMEMAILDIR = os.path.join(os.environ["HOME"], "Maildir")
MAILDIR = os.path.expanduser(os.environ.get("MAILDIR", HOMEMAILDIR))
mdir = MdFolder(MAILDIR)

# Depends on cmdlin
import cmdln

class MdCLI(cmdln.Cmdln):
    name = "md"

    def do_ls(self, subcmd, opts, folder=""):
        """List messages in the specified folder"""
        client = MdClient(MAILDIR)
        client.ls(foldername=folder,stream=sys.stdout)

    def do_lisp(self, subcmd, opts, folder=""):
        """List messages in the specified folder in JSON format"""
        client = MdClient(MAILDIR)
        client.lisp(foldername=folder,stream=sys.stdout)

    def do_make(self, subcmd, opts, path):
        """Make a maildir at the specified path.

        If the path is relative then create under MAILDIR
        else create at the absolute location.
        """
        d = path[0] if path[0][0] == "/" else joinpath(MAILDIR, path[0])
        os.makedirs(joinpath(d, "cur"))
        os.makedirs(joinpath(d, "new"))
        os.makedirs(joinpath(d, "tmp"))

    def do_text(self, subcmd, opts, message):
        """Get the best text part of the specified message"""
        client = MdClient(MAILDIR)
        client.gettext(message, sys.stdout)

    def do_struct(self, subcmd, opts, message):
        """Get the structure of the specified message"""
        client = MdClient(MAILDIR)
        client.getstruct(message, sys.stdout)

    def do_shell(self, subcmd, opts):
        """Run a shell for md"""
        # TODO fix this because it's broken right now
        shell = MdCLI()
        mdcli.main(argv=[], loop=cmdln.LOOP_ALWAYS)

if __name__ == "__main__":
    mdcli = MdCLI()
    sys.exit(mdcli.main())

# End
