"""
Maildir API.

Includes a folder object and a Message object.

The standard python versions of these fits in well with Python mail
libs but isn't particularly performant for a range of interesting
tasks. Hence this library.

"""

import re
from os import stat
from os.path import abspath
from os.path import basename
from os.path import join as joinpath

from email.parser import Parser
from email.parser import BytesParser
from email.utils import parsedate_tz
from email.utils import parsedate
from email.utils import mktime_tz

from datetime import datetime
from io import StringIO
from io import BytesIO

import time

from mdlib.hdrparser import HeaderOnlyParser
_hdr_parser = HeaderOnlyParser()

from pyproxyfs import Filesystem
OSFILESYSTEM = Filesystem()
MDMSG_FILENAME_PATTERN = "(?P<key>[0-9]+\\.[A-Za-z0-9]+)(\\.(?P<hostname>[.A-Za-z0-9-]+))*(:[2],(?P<flags>[PRSTDF]*))*"
SEPERATOR="#"

class MdMessage(object):
    def __init__(self, 
                 key, 
                 filename="", 
                 folder=None, 
                 filesystem=OSFILESYSTEM):
        # This is a cache var for implementing self.content
        self._content = None

        # In the future we may support building messages from strings
        # but right now we require a file
        if not filename:
            raise Exception("no filename specified")

        # Start by JUST reading the headers
        with filesystem.open(filename) as hdrs_fd:
            self.msgobj = _hdr_parser.parse(hdrs_fd, headersonly=True)

        self.headers_only = True

        self.folder = folder
        self.filename = filename
        self.filesystem = filesystem
        self.msgpathre = re.compile("%s/%s" % (
                joinpath(
                    self.folder.base,
                    self.folder.folder, 
                    "cur"
                    ),
                MDMSG_FILENAME_PATTERN
                ))
        self.key = key
        self.hdrmethodre = re.compile("get_([a-z_]+)")
        try:
            d = parsedate_tz(self.msgobj["Date"])
            t = mktime_tz(d)
            self.time = t
        except Exception as e:
            try:
                self.time = time.mktime(parsedate(self.msgobj["Date"]))
            except Exception as e:
                self.time = -1

    def _get_content(self):
        # self.content is provided by __getattr__ through the cache var self._content
        p = BytesParser()
        content = self.content
        content_io = BytesIO(content)
        parsed_msg = p.parse(content_io)
        return parsed_msg

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
                with self.filesystem.open(self.filename, "r+b") as msgfd:
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
            if since and since > 0.0:
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
            if since and since > 0.0:
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

    def __init__(self, foldername, base="", subfolder=False, filesystem=None):
        self.base = base
        self.folder = foldername
        self.is_subfolder = subfolder
        self.filesystem = filesystem or OSFILESYSTEM

        self._cur = joinpath(base, (".%s" if subfolder else "%s") % foldername, "cur")
        self._cur_re = re.compile("%s/%s" % (self._cur, MDMSG_FILENAME_PATTERN))

        # Memoization cache
        self._foldername_cache = {}  ### this is a cache of other folder names, eg: 'new'
        self._files_cache = {}
        self._keys_cache = _KeysCache()

    def get_name(self):
        """What is this folder's name?
        
        The name does include a "." if the folder is a subfolder.
        """
        return ".%s" % self.folder if self.is_subfolder else self.folder

    def _foldername(self, additionalpath=""):
        """Dot decorate a folder name."""
        if not self._foldername_cache.get(additionalpath):
            fn = joinpath(self.base, self.folder, additionalpath) \
                if not self.is_subfolder \
                else joinpath(self.base, ".%s" % self.folder, additionalpath)
            self._foldername_cache[additionalpath] = fn
        return self._foldername_cache[additionalpath]

    def folders(self):
        """Return a map of the subfolder objects for this folder.

        This is a snapshot of the folder list at the time the call was made. 
        It does not update over time.

        The map contains MdFolder objects:

          maildir.folders()["Sent"]

        might retrieve the folder .Sent from the maildir.
        """
        entrys = self.filesystem.listdir(abspath(self._foldername()))
        regex = re.compile("\\..*")
        just_dirs = dict([(d,d) for d in entrys if regex.match(d)])

        folder = self._foldername()
        filesystem = self.filesystem

        class FolderList(object):
            def __iter__(self):
                dirs = list(just_dirs.keys())
                dirs.sort()
                dirs.reverse()
                for dn in dirs:
                    yield MdFolder(
                        dn[1:],
                        base=folder,
                        subfolder=True,
                        filesystem=filesystem
                        )
                return

            def __list__(self):
                return [dn[1:] for dn in just_dirs]

            def __contains__(self, name):
                return just_dirs.__contains__(".%s" % name)

            def __getitem__(self, name):
                return MdFolder(
                    just_dirs[".%s" % name][1:],
                    base=folder,
                    subfolder=True,
                    filesystem=filesystem
                    )

        f = FolderList()
        return f

    def move(self, key, folder):
        """Move the specified key to folder.

        folder must be an MdFolder instance. MdFolders can be obtained
        through the 'folders' method call.
        """
        # Basically this is a sophisticated __delitem__
        # We need the path so we can make it in the new folder
        path, host, flags = self._exists(key)
        self._invalidate_cache()

        # Now, move the message file to the new folder
        newpath = joinpath(
            folder.base, 
            folder.get_name(), 
            "cur",     # we should probably move it to new if it's in new
            basename(path)
            )
        self.filesystem.rename(path, newpath)
        # And update the caches in the new folder
        folder._invalidate_cache()

    def __repr__(self):
        return "<%s>" % (self.folder)

    def _muaprocessnew(self):
        """Moves all 'new' files into cur, correctly flagging"""
        foldername = self._foldername("new")
        files = self.filesystem.listdir(foldername)
        for filename in files:
            if filename == "":
                continue
            curfilename = self._foldername(joinpath("new", filename))
            newfilename = joinpath(
                self._cur,
                "%s:2,%s" % (filename, "")
                )
            self.filesystem.rename(curfilename, newfilename)

    def _fileslist(self):
        if not self._files_cache:
            self._muaprocessnew()
            foldername = self._foldername("cur")
            files = self.filesystem.listdir(foldername)
            for filename in files:
                try:
                    # We could use "%s/%s" here instead of joinpath... it's faster
                    # path = joinpath(foldername, filename)
                    path = "%s/%s" % (foldername, filename)
                    m = self._cur_re.match(path)
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
        return list(self._fileslist()[0].keys())

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
        except KeyError as e:
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

# End
