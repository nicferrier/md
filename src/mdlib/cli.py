# md - a simple maildir command line user agent
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
md - A maildir command line user agent.

Copyright (C) 2010  Nic Ferrier <nic@ferrier.me.uk>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

__author__ = "Nic Ferrier <nic@ferrier.me.uk>"
__version__ = 0.1

import os.path
from os.path import join as joinpath
import os
import sys
import re

from mdlib import __version__ as md_version
from mdlib import MdClient

## This should be redefined as an option and a thread local
## Default value should come from an env var or be ~/Maildir
HOMEMAILDIR = "~/Maildir"
MAILDIR = os.path.expanduser(os.environ.get("MAILDIR", HOMEMAILDIR))

# Depends on cmdlin
from mdlib.cmdln import Cmdln
from mdlib.cmdln import option


class MdCLI(Cmdln):
    name = "md"
    filesystem = None
    stdout = sys.stdout
    stderr = sys.stderr

    def get_optparser(self):
        """Override to allow specification of the maildir"""
        p = Cmdln.get_optparser(self)
        p.add_option(
            "-M",
            "--maildir",
            action="store",
            dest="maildir"
            )
        p.add_option(
            "-V",
            "--verbose",
            action="store_true",
            dest="verbose"
            )
        return p

    @property
    def maildir(self):
        return getattr(self.options, "maildir", MAILDIR) or MAILDIR

    def do_version(self, subcmd, opts):
        """${cmd_name}: what version are we?"""
        print(md_version, file=self.stdout)

    def do_lsfolders(self, subcmd, opts):
        """${cmd_name}: list the sub folders of the maildir.

        ${cmd_usage}
        """
        client = MdClient(self.maildir, filesystem=self.filesystem)
        client.lsfolders(stream=self.stdout)

    @option("-r", "--reverse", help="reverse the listing", action="store_true")
    @option("-f", "--field", help="specify the field")
    @option("-g", "--grep", help="pattern match on the output string")
    @option(
        "-s", 
        "--since", 
        help="""only list the mails since this timestamp""", 
        action="store",
        default=-1
        )
    def do_ls(self, subcmd, opts, folder=""):
        """${cmd_name}: list messages in the specified folder

        ${cmd_usage}
        ${cmd_option_list}
        SINCE can be used with epoch times, for example: 

          md ls -s $(date '+%s')
        """
        client = MdClient(self.maildir, filesystem=self.filesystem)
        client.ls(
            foldername = folder, 
            stream = self.stdout, 
            reverse = getattr(opts, "reverse", False),
            grep = getattr(opts, "grep", None),
            field = getattr(opts, "field", None),
            since = float(getattr(opts, "since", -1))
            )

    @option("-r", "--reverse", help="reverse the listing", action="store_true")
    @option(
        "-s", 
        "--since", 
        help="""only list the mails since this timestamp""", 
        action="store",
        default=-1
        )
    def do_lisp(self, subcmd, opts, folder=""):
        """${cmd_name}: list messages in the specified folder in JSON format

        ${cmd_usage}
        """
        client = MdClient(self.maildir, filesystem=self.filesystem)
        client.lisp(
            foldername=folder,
            stream=self.stdout, 
            reverse=getattr(opts, "reverse", False),
            since=float(getattr(opts, "since", -1))
            )

    def do_make(self, subcmd, opts, path):
        """${cmd_name}: make a maildir at the specified path.

        ${cmd_usage}

        If the path is relative then create under MAILDIR
        else create at the absolute location.
        """
        # Do we need to make this ".path" if it's relative?
        d = path if path[0] == "/" else joinpath(self.maildir, "." + path)
        os.makedirs(joinpath(d, "cur"))
        os.makedirs(joinpath(d, "new"))
        os.makedirs(joinpath(d, "tmp"))
        os.makedirs(joinpath(d, "store"))

    def do_rm(self, subcmd, opts, message):
        """${cmd_name}: remove the specified message

        ${cmd_usage}
        """
        maildir = self.maildir
        client = MdClient(maildir, filesystem=self.filesystem)
        try:
            client.remove(message)
        except KeyError:
            return 1

    def do_mv(self, subcmd, opts, message, folder):
        """${cmd_name}: move the specified message to the specified folder

        ${cmd_usage}
        """
        client = MdClient(self.maildir, filesystem=self.filesystem)
        client.move(message, folder)

    def do_text(self, subcmd, opts, message):
        """${cmd_name}: get the best text part of the specified message

        ${cmd_usage}
        """
        client = MdClient(self.maildir, filesystem=self.filesystem)
        client.gettext(message, self.stdout)

    def do_raw(self, subcmd, opts, message):
        """${cmd_name}: dump the complete raw message

        ${cmd_usage}
        """
        client = MdClient(self.maildir)
        client.getraw(message, self.stdout)

    @option("-p", "--part", help="specify the part number")
    def do_rawpart(self, subcmd, opts, message):
        """${cmd_name}: dump a part from the specified message

        ${cmd_usage}
        ${cmd_option_list}
        """
        client = MdClient(self.maildir, filesystem=self.filesystem)
        partid = getattr(opts, "part", None)
        if not partid:
            client.getrawpart(message, self.stdout)
        else:
            client.getrawpartid(message, partid, self.stdout)

    @option("-j", "--json", help="show the parts in json structure", action="store_true")
    def do_struct(self, subcmd, opts, message):
        """${cmd_name}: get the structure of the specified message

        ${cmd_usage}
        ${cmd_option_list}
        """
        client = MdClient(self.maildir, filesystem=self.filesystem)
        as_json = getattr(opts, "json", False)
        client.getstruct(message, as_json=as_json, stream=self.stdout)

    def do_file(self, subcmd, opts, message):
        """${cmd_name}: download the whole file of the message.
        
        ${cmd_usage}
        """
        client = MdClient(self.maildir, filesystem=self.filesystem)
        client.get(message, self.stdout)

    def do_quit(self, subcmd, opts):
        """${cmd_name}: quit the shell.

        ${cmd_usage}
        """
        self.stop = True

    def do_shell(self, subcmd, opts):
        """${cmd_name}: run a shell for md.

        ${cmd_usage}

        The MAILDIR cannot be set with this command except through the
        environment variable.
        """
        self.cmdloop(intro=__doc__ + "\nType 'help' for help, 'quit' to quit.\n")

    @option("-N", "--noop", help="do not pull", action="store_true")
    @option("-f", "--filter", help="filter filename", action="store")
    def do_pull(self, subcmd, opts, remote_maildir):
        """${cmd_name}: pull the remote maildir into the local maildir.

        ${cmd_usage}
        ${cmd_option_list}

        The REMOTE_MAILDIR is a url which specifies where the dir
        is. A few different forms are supported:

          ssh://user@hostname/path   is a remote directory at path, accessed via ssh
          file://path                is a local maildir directory at path
        """
        import mdlib.pull 
        m = re.match(
            "(?P<protocol>[a-z]+)://(?P<urlpart>.*)",
            remote_maildir
            )
        if not m:
            print("md pull: the remote maildir url was unrecognized", file=self.stderr)
            return

        local_maildir = self.maildir
        noop = getattr(opts, "noop", False) or False
        verbose = getattr(self.options, "verbose", False) or False
        filterfile = getattr(opts, "filter", None) or None

        try:
            filterfd = open(filterfile)
        except:
            filterfd = None
            # Some error loading the filterfile
            if verbose:
                print("md pull: could not load filter file", file=self.stderr)

        data = m.groupdict()
        if data.get("protocol") == "ssh":
            m = re.match(
                "(?P<user>[a-zA-Z0-9-]+@)*(?P<hostname>[a-zA-Z0-9.-]+)(?P<path>[a-zA-Z0-9./-]+)",
                data.get("urlpart")
                )
            if not m:
                print("md pull: %s was not a remote maildir" % remote_maildir, file=self.stderr)
                return
            data = m.groupdict()
            host = data.get("hostname", None) \
                if not data.get("user", None) \
                else "%s@%s" % (data.get("user"), data.get("hostname"))
            remote_maildir = data.get("path")
            mdlib.pull.sshpull(host, remote_maildir, local_maildir, noop, verbose, filterfd)
        elif data.get("protocol") == "file":
            maildir = data.get("urlpart")
            mdlib.pull.filepull(maildir, local_maildir, noop, verbose, filterfd)
        else:
            print("md pull: %s not a recognized protocol" % protocol, file=self.stderr)

        # Finally try and close the filterfd
        if filterfd:
            try:
                filterfd.close()
            except:
                if verbose:
                    print("md pull: couldn't close open filter file", file=self.stderr)

    def do_newfilter(self, subcmd, opts):
        """${cmd_name}: make a filterfile and spit it to stdout.
        """
        from mdlib.filterprocessor import RULES
        print(RULES, file=self.stdout)

    def do_storecheck(self, subcmd, opts):
        """${cmd_name}: checks the store for files that may not be in the maildirs.
        """
        from os.path import basename
        from os.path import dirname
        from os.path import exists as existspath
        from os.path import islink
        from os.path import join as joinpath
        maildir = self.maildir
        cur = joinpath(maildir, "cur")
        new = joinpath(maildir, "new")
        store = joinpath(maildir, "store")
        
        found_list = []
        # Loop through the folders checking that everything maps back to the store
        for scandir in [cur, new]:
            for f in os.listdir(scandir):
                filename = joinpath(scandir, f)
                try:
                    assert islink(filename)
                    store_location = os.readlink(filename)
                    assert existspath(store_location) and dirname(store_location) == store
                except AssertionError:
                    print("%s was not a link into the store" % (
                            "/".join([
                                    filename.split("/")[-2],
                                    filename.split("/")[-1]
                                    ])
                            ), 
                          file=self.stdout)
                else:
                    found_list.append(basename(store_location))

        for storefile in os.listdir(store):
            if storefile not in found_list:
                print(
                    "%s found in store but not folders" % joinpath("store", storefile), 
                    file=self.stdout
                    )

class ShellCLI(MdCLI):
    def do_quit(self, subcmd, opts):
        """${cmd_name}: quit the shell.
        """
        self.stop = True


from mdlib.cmdln import LOOP_ALWAYS
from mdlib.cmdln import LOOP_NEVER

def main(*argv, 
          filesystem=None, 
          do_exit=True,
          stdout=None,
          stderr=None):
    """Main method for the cli.

    We allow the filesystem to be overridden for test purposes."""
    try:
        mdcli = MdCLI()
        mdcli.filesystem = filesystem
        mdcli.stdout = stdout or sys.stdout
        mdcli.stderr = stderr or sys.stderr
        retval = mdcli.main(*argv, loop=LOOP_NEVER)
        if do_exit:
            sys.exit(retval)
        else:
            return retval
    except KeyboardInterrupt:
        pass

# End
