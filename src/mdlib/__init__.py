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
__version__ = "0.81"

import sys
import re
from os.path import join as joinpath
from os.path import split as splitpath

import errno
from email.utils import parseaddr

try:
    import json
except ImportError:
    import simplejson as json

import logging
from mdlib.api import MdFolder
from mdlib.api import SEPERATOR

logger = logging.getLogger("mdlib")
logging.basicConfig()

def _escape(match_obj):
    if match_obj.group(0) == "\n":
       return ""
    if match_obj.group(0) == "\"":
        return "\\\""
    return

def _get_charset(content_type):
    """The feed parser we're using seems to not set charsets correctly.

    This is a little method to parse a content type and return the charset.
    """
    m = re.match("""[^;]+(; charset="*([A-Za-z0-9-]+)"*$)""", content_type)
    if m:
        return m.group(2)

class MdClient(object):
    def __init__(self, maildir, filesystem=None):
        self.logger = logging.getLogger("MdClient.%s" % maildir)
        # Why would we do this? it requires that maildir's have to end
        # in a slash. Bad idea? What benefit does it bring??
        ## foldername = maildir.split("/")[-1]
        ## base = splitpath(maildir)[0]
        # Instead of doing that, let's do this:
        foldername = ""
        base = maildir
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

        The returned list is: foldername, message key, message object
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

    def ls(self, foldername="INBOX", reverse=False, since=None, grep=None, field=None, stream=sys.stdout):
        """Do standard text list of the folder to the stream.

        'foldername' is the folder to list.. INBOX by default.

        'since' allows the listing to be date filtered since that
        date. It should be a float, a time since epoch.
        
        'grep' allows text matching on the whole record

        'field' allows only 1 field to be output
        """
        if foldername == "":
            foldername = "INBOX"

        msg_list = self._list(foldername, reverse, since)
        for folder, mk, m in msg_list:
            try:
                # I am very unsure about this defaulting of foldername
                output_items = (
                    "%s%s%s" % (folder.folder or foldername or "INBOX", SEPERATOR, mk),
                    m.date,
                    m.get_from()[0:50] if m.get_from() else "", 
                    m.get_flags(),
                    re.sub("\n", "", m.get_subject() or "")
                    )

                output_string = "% -20s % 20s % 50s  [%s]  %s" % output_items
                if not grep or (grep and grep in output_string):
                    if field:
                        print(output_items[int(field)], file=stream)
                    else:
                        print(output_string, file=stream)
            except IOError as e:
                if e.errno == errno.EPIPE:
                    # Broken pipe we can ignore
                    return
                self.logger.exception("whoops!")
            except Exception as e:
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
                print(json.dumps({
                        'folder': folder.folder or foldername or "INBOX",
                        'key': "%s%s%s" % (folder.folder or foldername or "INBOX", SEPERATOR, mk),
                        'date':  str(m.date),
                        "flags": m.get_flags(),
                        'from': fromval(m.get_from()),
                        'subject': re.sub("\n|\'|\"", _escape, m.get_subject() or "")
                        }), file=stream)
            except IOError as e:
                if e.errno == errno.EPIPE:
                    # Broken pipe we can ignore
                    return
                self.logger.exception("whoops!")
            except Exception as e:
                self.logger.exception("whoops!")

    def lsfolders(self, stream=sys.stdout):
        """List the subfolders"""
        for f in self.folder.folders():
            print(f.folder.strip("."), file=stream)

    def remove(self, msgid):
        foldername, msgkey = msgid.split(SEPERATOR)
        folder = self.folder if foldername == "INBOX" else self._getfolder(foldername)
        del folder[msgkey]

    def move(self, msgid, to_foldername):
        foldername, msgkey = msgid.split(SEPERATOR)
        folder = self.folder if foldername == "INBOX" else self._getfolder(foldername)
        target_folder = folder.folders()[to_foldername]
        folder.move(msgkey, target_folder)

    def _get(self, msgid):
        """Yields the message header against each part from the message."""
        foldername, msgkey = msgid.split(SEPERATOR)
        folder = self.folder if foldername == "INBOX" else self._getfolder(foldername)
        # Now look up the message
        msg = folder[msgkey]
        msg.is_seen = True
        hdr = list(msg.items())
        for p in msg.walk():
            yield hdr,p
        return

    def gettext(self, msgid, stream=sys.stdout, splitter="--text follows this line--\n"):
        """Get the first text part we can find and print it as a message.

        This is a simple cowpath, most of the time you want the first plain part.

        'msgid' is the message to be used
        'stream' is printed to with the header, splitter, first-textpart
        'splitter' is text used to split the header from the body, Emacs uses this
        """
        for hdr,part in self._get(msgid):
            if part.get_content_type() == "text/plain":
                for name,val in hdr:
                    # Use the subtype, since we're printing just that - tidy it up first
                    if name.lower() == "content-type":
                        val = part["content-type"]
                    val = " ".join([l.strip() for l in val.split("\n")])
                    print("%s: %s" % (name,val), file=stream)
                print(splitter, file=stream)
                payload = part.get_payload(decode=True)
                # There seems to be a problem with the parser not doing charsets for parts
                chartype = part.get_charset() \
                    or _get_charset(part.get("Content-Type", "")) \
                    or "us-ascii"
                print(payload.decode(chartype), file=stream)
                break

    def getrawpart(self, msgid, stream=sys.stdout):
        """Get the first part from the message and print it raw.
        """
        for hdr, part in self._get(msgid):
            pl = part.get_payload(decode=True)
            if pl != None:
                print(pl, file=stream)
                break

    def getrawpartid(self, msgid, partid, stream=sys.stdout):
        """Get a specific part from the message and print it raw.
        """
        parts = [part for hdr,part in self._get(msgid)]
        part = parts[int(partid)]
        pl = part.get_payload(decode=True)
        if pl != None:
            print(pl, file=stream)


    def getraw(self, msgid, stream=sys.stdout):
        """Get the whole message and print it.
        """
        foldername, msgkey = msgid.split(SEPERATOR)
        folder = self.folder if foldername == "INBOX" else self._getfolder(foldername)
        msg = folder[msgkey]
        print(msg.content)

    def getstruct(self, msgid, as_json=False, stream=sys.stdout):
        """Get and print the whole message.

        as_json indicates whether to print the part list as JSON or not.
        """
        parts = [part.get_content_type() for hdr, part in self._get(msgid)]
        if as_json:
            print(json.dumps(parts), file=stream)
        else:
            for c in parts:
                print(c, file=stream)

    def get(self, msgid, stream=sys.stdout):
        foldername, msgkey = msgid.split(SEPERATOR)
        folder = self.folder if foldername == "INBOX" else self._getfolder(foldername)
        # Now look up the message
        msg = folder[msgkey]
        msg.is_seen = True
        print(msg.as_string(), file=stream)

# End
