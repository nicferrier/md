#!/usr/bin/python

"""md.py command
commands:

 ls [folder ...]
    list the messages in the specified folders (INBOX by default) An
    alternative to this command is "lsjson" which does the same thing
    but returns json.

 folders    
    list the folders, except the inbox.

 get <tag> 
    show the message.

 getstruct <tag>
    show the structure of the message

 gettext <tag>
    shows the first text part it can find.

Author: nic ferrier - nferrier@tapsellferrier.co.uk
"""

import mailbox
import sys
import re
import os
from os.path import exists
from os.path import join as joinpath
from os.path import split as splitpath

import time
from datetime import datetime

from email.parser import FeedParser
from email.parser import HeaderParser
from email.utils import parsedate_tz
from email.utils import parsedate
from email.utils import parseaddr
from email.utils import mktime_tz

import simplejson
import pdb
import memcache
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

class _MdMessage(mailbox.MaildirMessage):
    """An extension of standard maildir message that irons out bad behaviour.

    The message records the maildir it came from. It can have it's
    maildir set so you don't need to regenerate.
    """
    def __init__(self, message=None, folder=None):
        """Pass the file's header only"""
        self._key = None
        if folder:
            self._folder = folder._path
        msgobj = hdr_parser.parse(message, headersonly=True)
        mailbox.MaildirMessage.__init__(self, msgobj)
        try:
            d=parsedate_tz(msgobj["date"])
            t=mktime_tz(d)
            self._date = t
        except Exception:
            try:
                self._date = time.mktime(parsedate(msgobj["date"]))
            except:
                pass

    def set_maildir(self, maildir):
        self._folder = maildir._path

    def get_maildir(self):
        global folderlist
        maildir = folderlist.get(self._folder)
        if not maildir:
            logger.debug("could not find %s in %s" % (self._folder, folderlist))
            maildir = Md(self._folder,
                         factory=_MdMessage,
                         create=False)

        logger.debug("get_maildir: %s with get for %s" % (
                maildir._path, 
                repr(maildir)
                ))
        return maildir

    def set_key(self, key):
        self._key = key

    def set_subdir(self, subdir):
        if subdir != self._subdir:
            maildir = self.get_maildir()
            if maildir:
                specific_path = maildir._lookup(self._key)
                msg_path = joinpath(maildir._path, specific_path)
                if exists(msg_path):
                    new_specific = joinpath(
                        subdir,
                        *splitpath(specific_path)[1:]
                        )
                    os.rename(
                        msg_path, 
                        joinpath(maildir._path, new_specific)
                        )
                    self._subdir = subdir

class Md(mailbox.Maildir):
    def __init__(self, *args, **kwargs):
        mailbox.Maildir.__init__(self, *args, **kwargs)
        self._last_read = None
        self._new_path = joinpath(self._path, 'new')
        self._cur_path = joinpath(self._path, 'cur')
        global folderlist
        folderlist[self._path] = self

    def __str__(self):
        return "%s %s" % (repr(self), self._path)

    def get_folder(self, folder):
        path = joinpath(self._path, "." + folder)
        global folderlist
        #logger.error("%s with get for %s" % (id(self), path))
        maildirfolder = folderlist.get(path, 
                                       Md(path,
                                          factory=self._factory,
                                          create=False))
        return maildirfolder
        

    def _refresh(self):
        #logger.error("refresh: %s %s" % (self, self._last_read))
        #logger.error("refresh: %s" % folderlist)
        new_mtime = os.path.getmtime(self._new_path)
        cur_mtime = os.path.getmtime(self._cur_path)
        
        if self._last_read is not None \
                and new_mtime <= self._last_read \
                and cur_mtime <= self._last_read:
            #logger.error("%s skipping refresh" % self)
            return

        mailbox.Maildir._refresh(self)
        self._last_read = time.time()
        #logger.error("refresh (end): %s %s" % (self, self._last_read))


## This should be redefined as an option and a thread local
## Default value should come from an env var or be ~/Maildir
HOMEMAILDIR = os.path.join(os.environ["HOME"], "Maildir")
MAILDIR = os.environ.get("MAILDIR", HOMEMAILDIR)
mdir = Md(MAILDIR, factory=_MdMessage)

# The Maildir seen flag
SEEN_FLAG="S"
TRASHED_FLAG="T"

def _lisp(*args):
    return "(%s)" % " ".join(("\"%s\"" % str(x) for x in args))

def help(*args):
    print __doc__

def _getcache():
    return memcache.Client(servers=["127.0.0.1:11211"])

def folders(*args):
    """Returns folders. Does not return INBOX currently"""
    for folder in ["INBOX"] + mdir.list_folders():
        print folder

def inbox_folder_guard(mdir, folder_name, factory=_MdMessage):
    """Helps with abstracting the inbox"""
    mdir = Md(MAILDIR, factory)
    if folder_name == "INBOX":
        return mdir
    else:
        return mdir.get_folder(folder_name)

def mkfolder(folder_name=[]):
    """Make a new folder"""
    # new_folder_re = "((([A-Za-z0-9_-]+)\\.)*)([A-Za-z0-9_-]+)"
    mdir.add_folder(folder_name[0])

def escape(match_obj):
    if match_obj.group(0) == "\n":
       return ""
    if match_obj.group(0) == "\"":
        return "\\\""
    return

def _list(folders=["INBOX"]):
    """Base folder lister.

    This is used by the lister frontends like ls and lisp.

    It stores retrieved messages in memcache for later pulling."""

    if folders == []:
        folders = ["INBOX"]
    mdirs = [(folder, inbox_folder_guard(mdir, folder)) \
                 for folder in folders]

    cache = _getcache()
    #pdb.set_trace()
    for folder,maildir in mdirs:
        mks = maildir.keys()
        cached_msgs = cache.get_multi(mks, "md%s" % folder if folder else "INBOX")

        logger.error("got %d" % len(cached_msgs.keys()))
        new_msgs = {}
        msgs = []
        cache_hits = 0
        for mk in mks:
            #pdb.set_trace()
            msg = cached_msgs.get("%s" % mk)
            if not msg:
                msg = maildir.get(mk)
                msg._key = mk
                msg.set_maildir(maildir)
                new_msgs[mk] = msg

                # Move it to the correct subdir if necessary
                if msg.get_subdir() == "new":
                    msg.set_subdir("cur")

                # Cache it: this is relatively inefficient
                cache.set(
                    "md%s%s" % (folder if folder else "INBOX", mk), 
                    msg)
            else:
                cache_hits += 1

            # Build the list of messages
            try:
                msg._key = mk
            except AttributeError:
                logger.error("key problem")

            msgs += [msg]

        # FIXME:: this doesn't work all the time
        #  Store the new ones in memcache
        #   c.set_multi(new_msgs, key_prefix="md")

        # Now sort them and spit them out
        msgs.sort(key=mailbox.MaildirMessage.get_date)
        for msg in msgs:
            yield folder, msg._key, msg

        # And finally print cache performance
        logger.info("cache hits: %d" % cache_hits)

def ls(folders=["INBOX"]):
    """Outputs simple state of a folder

    This is designed to be called from shell and processed with awk
    and other such tools.

    Each message is output with a message key which can be used to 
    retrieve the message later."""

    for folder, mk, m in _list(folders):
        try:
            print "% 40s % 40s % 50s [%s] %s" % (
                "%s%s" % (folder, mk),
                datetime.fromtimestamp(m.get_date()),
                m["from"][0:50] if m["from"] else "", 
                m.get_flags(),
                re.sub("\n", "", m["subject"] or ""))
        except Exception,e:
            logger.error("whoops! %s" % (str(e)))


def lisp(folders=None):
    return lsjson(folders)

def lsjson(folders=None):
    """Outputs state of a folder as a JSON structure for each message.
    Each message is output with a message key which can be used to 
    retrieve the message later.

    Why is this method called LISP? Originally this outputted a
    property list, a LISP data structure. It's not quicker and easier
    to get it to output JSON... but JSON is so inspired by LISP that
    retaining the name seemed ok."""

    def fromval(hdr):
        if hdr:
            return parseaddr(hdr)

    for folder, mk, m in _list(folders):
        try:
            print simplejson.dumps({
                    'folder': folder,
                    'key': "%s%s" % (folder, mk),
                    'date':  str(datetime.fromtimestamp(m.get_date())),
                    "flags": m.get_flags(),
                    'from': fromval(m["from"]),
                    'subject': re.sub("\n|\'|\"", escape, m["subject"] or "")
                    })
        except Exception, e:
            pass

import commands

def fetch(folders=None):
    for folder in folders:
        cmdstr = """rsync -ae ssh \
 --out-format "%%n" \
 --delete %s/.%s %s | grep new""" % (
            os.environ["MDPICKUPURL"],
            folder,
            os.path.join(mdir._path, folder))
        output = commands.getoutput(cmdstr)
        print "imported %d into %s" % (len(output.split("\n")), folder)
    
def _messageop(messages=None, subjectfilter=None):
    cache = _getcache()
    # Does message awareness via the embedded key
    folders = mdir.list_folders() + ["INBOX"]
    for message_key in messages:
        for folder in folders:
            m = re.match("%s(.*)" % folder, message_key)
            if m:
                key = m.group(1)
                maildir = inbox_folder_guard(mdir, folder, factory=None)
                message = maildir.get_message(key)
                message._folder = maildir._path
                message._key = key
                # Some standard filtering
                if subjectfilter and re.search(subjectfilter, message["subject"]):
                    continue

                # Return the message
                yield message_key, message

                # Need to check it hasn't been deleted
                try:
                    # Update the maildir if the message has been updated
                    maildir[key]
                    maildir[key] = message
                    cache.set("md%s" % message_key, message)
                except KeyError:
                    pass

def getstruct(messages=None, subjectfilter=None):
    for message_key, msg in _messageop(messages, subjectfilter):
        print "%s ( %s )" % (
            message_key,
            " ".join([part.get_content_type() for part in msg.walk()]),
            )

def gettext(messages=None, subjectfilter=None):
    for message_key,msg in _messageop(messages, subjectfilter):
        msg.add_flag(SEEN_FLAG)
        for hdr,val in msg.items():
            print "%s: %s" % (hdr,val)
        for p in msg.walk():
            if p.get_content_type() == "text/plain":
                print p.as_string()
                break

def get(messages=None, subjectfilter=None):
    for message_key,msg  in _messageop(messages, subjectfilter):
        msg.add_flag(SEEN_FLAG)
        print msg

def trash(messages=None, subjectfilter=None):
    for message_key, msg in _messageop(messages, subjectfilter):
        folder = msg.get_maildir()
        folder.remove(msg._key)

if __name__ == "__main__":
    # We need option processing in here
    if sys.argv[1] in ["ls", "lisp", "lsjson", 
                       "mkfolder", "folders", 
                       "get", "getstruct", "gettext",
                       "trash",
                       "fetch",
                       "help"]:
        if len(sys.argv) > 2 and sys.argv[2] == "-":
            args = sys.stdin.read().split("\n")
        else:
            args = repr(sys.argv[2:])
        exec "%s(%s)" % (sys.argv[1], args)

# End
