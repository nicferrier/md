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
from os import stat
from os.path import abspath
from os.path import join as joinpath
from os.path import split as splitpath
import errno

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
import mmap

class HeaderOnlyParser(HeaderParser):
    """JUST parse the header

    Python's HeaderParser actually parses the whole message. Duh."""

    def parse(self, fp, headersonly=True):
        """Create a message structure from the data in a file."""
        feedparser = FeedParser(self._class)
        feedparser._set_headersonly()
        mp = mmap.mmap(fp.fileno(), 0, access=mmap.ACCESS_READ)
        data = ""
        while True:
            line = mp.readline()
            data = data + line
            if line == "\n":
                break
        feedparser.feed(data) # mp[0:5000])
        return feedparser.close()

_hdr_parser = HeaderOnlyParser()

from pyproxyfs import Filesystem
OSFILESYSTEM = Filesystem()
MDMSGPATHRE = "%s/(?P<key>[0-9]+\\.[A-Za-z0-9]+)(\\.(?P<hostname>[.A-Za-z0-9-]+))*(:[2],(?P<flags>[PRSTDF]*))*"
SEPERATOR="#"
class MdMessage(object):
    def __init__(self, 
                 key, 
                 filename="", 
                 folder=None, 
                 filesystem=OSFILESYSTEM):
        # This is a cache var for implementing self.content
        self._content = None

        # Start by JUST reading the headers
        with filesystem.open(filename) as hdrs_fd:
            self.msgobj = _hdr_parser.parse(hdrs_fd, headersonly=True)

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

    def _get_content(self):
        # self.content is provided by __getattr__ through the cache var self._content
        return Parser().parse(StringIO(self.content))

    def walk(self):
        if self.headers_only:
            self.msgobj = self._get_content()
        return self.msgobj.walk()

    def as_string(self):
        """Get the underlying message object as a string"""
        if self.headers_only:
            self.msgobj = self._get_content()

        # We could just use msgobj.as_string() but this is more flexible... we might need it.
        from email.generator import Generator
        fp = StringIO()
        g = Generator(fp, maxheaderlen=60)
        g.flatten(self.msgobj)
        text = fp.getvalue()
        return text

    def get_content_type(self):
        if self.headers_only:
            self.msgobj = self._get_content()
        return dict(self.msgobj._headers).get("Content_type", "text/plain")

    def __getattr__(self, attrname):
        """Implements the get methods for SMTP headers on the embedded message."""
        if attrname == "content":
            if not self._content:
                with self.filesystem.open(self.filename) as msgfd:
                    self._content = msgfd.read()
            return self._content

        # Otherwise access the header
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
        self.folder._invalidate_cache()
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

class _KeysCache(object):
    """A means of making the parsing of the messages lazy.

    It has to be lazy so we don't have to parse the entire folder when
    we only want to display a message.
    """
    def __init__(self):
        self.store = {}

    def __setitem__(self, key, o):
        self.store[key] = o

    def get(self, key, default):
        try:
            return self.__getitem__(key)
        except KeyError:
            return default

    def __getitem__(self, key):
        return self._get_message(key)

    def _get_message(self, key, since=None):
        """Return the MdMessage object for the key.

        The object is either returned from the cache in the store or
        made, cached and then returned.

        If 'since' is passed in the modification time of the file is
        checked and the message is only returned if the mtime is since
        the specified time. 

        If the 'since' check fails, None is returned.

        'since' must be seconds since epoch.
        """
        stored = self.store[key]
        if isinstance(stored, dict):
            filename = stored["path"]
            folder = stored["folder"]
            if since > 0.0:
                st = stat(filename)
                if st.st_mtime < since:
                    return None
            stored = MdMessage(
                key, 
                filename = filename, 
                folder = folder,
                filesystem = folder.filesystem
                )
            self.store[key] = stored
        else:
            if since > 0.0:
                st = stat(stored.filename)
                if st.st_mtime < since:
                    return None
                
        return stored

    def __iter__(self):
        """just the keys iterator"""
        return self.store.__iter__()

    def iteritems(self):
        for n in self.store.__iter__():
            v = self._get_message(n)
            yield n,v
        return

    def iteritems_since(self, since=None):
        for n in self.store.__iter__():
            v = self._get_message(n, since=since)
            if v:
                #print "%s %s %s" % (since,n,stat(v.filename).st_mtime)
                yield n,v
        return

    def items(self):
        return list(self.iteritems())

    def items_since(self, since=None):
        return list(self.iteritems_since(since=since))
            

class MdFolder(object):
    """A Maildir folder.

    Maildir is a lockless mail store.
    """

    def __init__(self, foldername, base="", subfolder=False, filesystem=OSFILESYSTEM):
        self.base = base
        self.folder = foldername
        self.is_subfolder = subfolder
        self.filesystem = filesystem

        # Memoization cache
        self._foldername_cache = {}
        self._files_cache = {}
        self._keys_cache = _KeysCache()

    def _foldername(self, additionalpath=""):
        if not self._foldername_cache.get(additionalpath):
            fn = joinpath(self.base, self.folder, additionalpath) \
                if not self.is_subfolder \
                else joinpath(self.base, ".%s" % self.folder, additionalpath)
            self._foldername_cache[additionalpath] = fn
        return self._foldername_cache[additionalpath]

    def folders(self):
        """Return an object the holds the folders for this folder.

        This is a snapshot of the folder list at the time the call was made. 
        It does not update over time.
        """

        entrys = self.filesystem.listdir(abspath(self._foldername()))
        regex = re.compile("\\..*")
        just_dirs = [d for d in entrys if regex.match(d)]

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

    def _fileslist(self):
        if not self._files_cache:
            self._muaprocessnew()
            foldername = self._foldername("cur")
            regex = re.compile(MDMSGPATHRE % foldername)
            files = self.filesystem.listdir(foldername)
            for filename in files:
                try:
                    # We could use "%s/%s" here instead of joinpath... it's faster
                    # path = joinpath(foldername, filename)
                    path = "%s/%s" % (foldername, filename)
                    m = regex.match(path)
                    if m:
                        desc = m.groupdict()
                        key = desc["key"]
                        self._files_cache[path] = desc
                        self._keys_cache[key] = {"path": path, "folder": self}
                except IOError:
                    # A big candidate for this could be you doing 
                    # one thing in one process (like removing a file)
                    # and this in another
                    pass

        return self._files_cache, self._keys_cache

    def _invalidate_cache(self):
        self._files_cache = {}
        self._keys_cache = _KeysCache()

    def _curlist(self):
        """The list of messages in 'cur'. Memoized"""
        return self._fileslist()[0].keys()

    def _curiter(self):
        """Just the iter of _curlist"""
        return self._curlist().__iter__()

    def _exists(self, key):
        """Find a key in a particular section

        Searches through all the files and looks for matches with a regex.
        """
        filecache, keycache = self._fileslist()
        msg = keycache.get(key, None)
        if msg:
            path = msg.filename
            meta = filecache[path]
            return path, meta["hostname"], meta.get("flags", "")
        raise KeyError("not found %s" % key)
            
    def __getitem__(self, key):
        try:
            filestore, keycache = self._fileslist()
            msgobj = keycache[key]
            return msgobj
        except KeyError, e:
            e.message = "no such message %s" % key
            raise

    def __delitem__(self, key):
        """Delete an item from the maildir."""
        path, host, flags = self._exists(key)
        self._invalidate_cache()
        self.filesystem.remove(path)

    def __iter__(self):
        filestore, keystore = self._fileslist()
        return keystore.__iter__()

    def iterkeys(self):
        return self.__iter__()

    def iteritems(self):
        for k in self.iterkeys():
            yield k, self[k]

    def keys(self):
        return list(self.iterkeys())

    def values(self):
        return list([self[k] for k in self.iterkeys()])

    def items(self):
        filestore, keystore = self._fileslist()
        return keystore.items()

    def items_since(self, since=None):
        filestore, keystore = self._fileslist()
        items = keystore.items_since(since=since)
        return items

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

    def _list(self, foldername="INBOX", reverse=False, since=None):
        """Do structured list output.

        Sorts the list by date, possibly reversed, filtered from 'since'.
        """
        folder = self.folder \
            if foldername == "INBOX" \
            else self._getfolder(foldername)

        def sortcmp(d):
            try:
                return d[1].date
            except:
                return -1

        lst = folder.items() if not since else folder.items_since(since)
        sorted_lst = sorted(lst, key=sortcmp, reverse=1 if reverse else 0)
        itemlist = [(folder, key, msg) for key,msg in sorted_lst]
        return itemlist

    def ls(self, foldername="INBOX", reverse=False, since=None, stream=sys.stdout):
        """Do standard text list of the folder to the stream.

        'since' allows the listing to be date filtered since that
        date. It should be a float, a time since epoch.
        """
        for folder, mk, m in self._list(foldername, reverse, since):
            try:
                # I am very unsure about this defaulting of foldername
                print >>stream, "% -20s % 20s % 50s  [%s]  %s" % (
                    "%s%s%s" % (folder.folder or foldername or "INBOX", SEPERATOR, mk),
                    m.date,
                    m.get_from()[0:50] if m.get_from() else "", 
                    m.get_flags(),
                    re.sub("\n", "", m.get_subject() or "")
                    )
            except IOError,e:
                if e.errno == errno.EPIPE:
                    # Broken pipe we can ignore
                    return
                self.logger.exception("whoops!")
            except Exception,e:
                self.logger.exception("whoops!")

    def lisp(self, foldername="INBOX", reverse=False, since=None, stream=sys.stdout):
        """Do JSON list of the folder to the stream.

        'since' allows the listing to be date filtered since that
        date. It should be a float, a time since epoch.
        """
        def fromval(hdr):
            if hdr:
                return parseaddr(hdr)

        for folder, mk, m in self._list(foldername, reverse, since):
            try:
                print >>stream, json.dumps({
                        'folder': folder.folder or foldername or "INBOX",
                        'key': "%s%s%s" % (folder.folder or foldername or "INBOX", SEPERATOR, mk),
                        'date':  str(m.date),
                        "flags": m.get_flags(),
                        'from': fromval(m.get_from()),
                        'subject': re.sub("\n|\'|\"", _escape, m.get_subject() or "")
                        })
            except IOError,e:
                if e.errno == errno.EPIPE:
                    # Broken pipe we can ignore
                    return
                self.logger.exception("whoops!")
            except Exception,e:
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

    def gettext(self, msgid, stream=sys.stdout, splitter="--text follows this line--\n"):
        """Get the first text part we can find and print it as a message.

        This is a simple cowpath, most of the time you want the first plan part.

        'msgid' is the message to be used
        'stream' is printed to with the header, splitter, first-textpart
        'splitter' is text used to split the header from the body
        """
        for hdr,part in self._get(msgid):
            if part.get_content_type() == "text/plain":
                for hdr,val in hdr:
                    print >>stream, "%s: %s" % (hdr,val)
                print >>stream, splitter
                print >>stream, part.get_payload(decode=True)
                break

    def getstruct(self, msgid, stream=sys.stdout):
        """Get and print the whole message.
        """
        for hdr,part in self._get(msgid):
            if part.get_content_type() == "text/plain":
                print >>stream, part.get_content_type()

    def get(self, msgid, stream=sys.stdout):
        foldername, msgkey = msgid.split(SEPERATOR)
        folder = self.folder if foldername == "INBOX" else self._getfolder(foldername)
        # Now look up the message
        msg = folder[msgkey]
        msg.is_seen = True
        print >>stream, msg.as_string()

# End
