#!/usr/bin/python

# mdlib - a simple maildir library.
# Copyright (C) 2010  Nic Ferrier <nic@ferrier.me.uk>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""
mdlib - A maildir and maildir client library .
"""

__author__= "nic ferrier - nic@ferrier.me.uk"
__version__ = "0.1"

import sys
import re
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

from StringIO import StringIO
import logging

logger = logging.getLogger("mdlib")
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
            curfilename = self._foldername(joinpath("new", filename))
            newfilename = joinpath(
                self._foldername("cur"),
                "%s:2,%s" % (filename, "")
                )
            self.filesystem.rename(curfilename, newfilename)

    def _curiter(self):
        """An iterator over the messages in 'cur'"""
        foldername = self._foldername("cur")
        for filename in self.filesystem.listdir(foldername):
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

    def __delitem__(self, key):
        """Delete an item from the maildir."""
        path, host, flags = self._exists(key)
        self.filesystem.remove(path)

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

    def remove(self, msgid):
        foldername, msgkey = msgid.split(SEPERATOR)
        folder = self.folder if foldername == "INBOX" else self._getfolder(foldername)
        del folder[msgkey]

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

    def getrawpart(self, msgid, stream=sys.stdout):
        """Get the first part from the message and print it raw.
        """
        for hdr, part in self._get(msgid):
            pl = part.get_payload(decode=True)
            if pl != None:
                print >>stream, pl
                break

    def getraw(self, msgid, stream=sys.stdout):
        """Get the whole message and print it.
        """
        foldername, msgkey = msgid.split(SEPERATOR)
        folder = self.folder if foldername == "INBOX" else self._getfolder(foldername)
        msg = folder[msgkey]
        print msg.content

    def getstruct(self, msgid, stream=sys.stdout):
        """Get and print the whole message.
        """
        for hdr,part in self._get(msgid):
            print >>stream, part.get_content_type()


# End
