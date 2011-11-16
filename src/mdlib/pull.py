#!/usr/bin/env python

"""Use ssh to retrieve remote maildir files.

This defines the flat local store for ensuring that we can synchronize
a changing remote end with a changing local end. All the actual
maildir files are symlinks into the store.

The store needs abstraction so it's not tied just to this ssh
method. It also needs maintenance commands to flush back local changes
to the upstream maildir. For example removing files on the upstream
that have no links into the maildir structure locally.

The protocol is implemented with basic gnu/sh commands:

  echo
  while
  read
  sed

the sed is gnu sed.
"""

from subprocess import Popen
from subprocess import PIPE

class _SSHStore(object):
    """A very simple ssh client based on the unix command.

    This could easily be made better using one of the python ssh
    implementations such as paramiko."""

    def __init__(self, host, directory):
        self.host = host
        self.directory = directory

    def cmd(self, cmd, verbose=False):
        """Executes the specified command on the remote host.

        The cmd must be format safe, this means { and } must be doubled, thusly:

          echo /var/local/maildir/{{cur,new}}

        the cmd can include the format word 'maildir' to be replaced
        by self.directory. eg:

          echo {maildir}/{{cur,new}}
        """
        command = cmd.format(maildir=self.directory)
        if verbose:
            print(command)
        p = Popen([
                "ssh",
                "-T",
                self.host,
                command
                ], stdin=PIPE, stdout=PIPE, stderr=PIPE)
        stdout,stderr = p.communicate()
        return stdout

class _Store(object):
    """A simple local based store command abstraction."""

    def __init__(self, directory):
        self.directory = directory

    def cmd(self, cmd, verbose=False):
        """Executes the specified command on the remote host.

        The cmd must be format safe, this means { and } must be doubled, thusly:

          echo /var/local/maildir/{{cur,new}}

        the cmd can include the format word 'maildir' to be replaced
        by self.directory. eg:

          echo {maildir}/{{cur,new}}
        """
        command = cmd.format(maildir=self.directory)
        if verbose:
            print(command)
        p = Popen(["bash", "-c", command], stdin=PIPE, stdout=PIPE, stderr=PIPE)
        stdout,stderr = p.communicate()
        return stdout


from os import listdir
from os import symlink
from os.path import abspath
from os.path import basename
from os.path import expanduser
from os.path import exists as existspath
from os.path import join as joinpath
import re

def _list_remote(store, maildir, verbose=False):
    """List the a maildir.

    store is an abstract representation of the source maildir. 

    maildir is the local maildir to which mail will be pulled.

    This is a generator for a reason. Because of the way ssh
    multi-mastering works a single open TCP connection allows multiple
    virtual ssh connections. So the encryption and tcp only has to be
    done once.

    If this command returned a list then the ssh list command would
    have finished and the ssh connection for each message would have
    to be made again.
    """
    # This command produces a list of all files in the maildir like:
    #   base-filename timestamp container-directory
    command = """echo {maildir}/{{cur,new}} | tr ' ' '\\n' | while read path ; do ls -1Ugo --time-style=+%s $path | sed -rne "s|[a-zA-Z-]+[ \t]+[0-9]+[ \t]+[0-9]+[ \t]+([0-9]+)[ \t]+([0-9]+\\.[A-Za-z0-9]+)(\\.([.A-Za-z0-9-]+))*(:[2],([PRSTDF]*))*|\\2 \\1 $path|p";done"""
    stdout = store.cmd(command, verbose)
    lines = stdout.split("\n")
    for line in lines:
        parts = line.split(" ")
        if len(parts) >= 3:
            yield parts[0:3]
    
def sshpull(host, maildir, localmaildir, noop=False, verbose=False, filterfile=None):
    """Pull a remote maildir to the local one.
    """
    store = _SSHStore(host, maildir)
    _pull(store, localmaildir, noop, verbose, filterfile)

def filepull(maildir, localmaildir, noop=False, verbose=False, filterfile=None):
    """Pull one local maildir into another.

    The source need not be an md folder (it need not have a store). In
    this case filepull is kind of an import.
    """
    store = _Store(maildir)
    _pull(store, localmaildir, noop, verbose, filterfile)

from .filterprocessor import parse as parse_filter
from io import StringIO
from .hdrparser import HeaderOnlyParser
from os.path import basename
from os.path import dirname
from .api import MdFolder

def _filter(msgdata, mailparser, mdfolder, mailfilters):
    """Filter msgdata by mailfilters"""
    if mailfilters:
        for f in mailfilters:
            msg = mailparser.parse(StringIO(msgdata))
            rule = f(msg, folder=mdfolder)
            if rule:
                yield rule
    return

def _pull(store, localmaildir, noop=False, verbose=False, filterfile=None):
    localstore = expanduser(joinpath(localmaildir, "store"))
    
    # Get the list of mail we already have locally
    maildir_pattern = re.compile(
        "^([0-9]+\\.[A-Za-z0-9]+)(\\.([.A-Za-z0-9-]+))*(:[2],([PRSTDF]*))*(.*)"
        )
    localfiles = [
        maildir_pattern.match(f).group(1) 
        for f in listdir(localstore) if maildir_pattern.match(f)
        ]

    # Read in the filters if we have them
    mailfilters = parse_filter(filterfile) if filterfile else []
    mailparser = HeaderOnlyParser() if mailfilters else None
    mdfolder = MdFolder(
        basename(localmaildir), 
        base=dirname(localmaildir)
        ) if mailfilters else None
    # Loop through the remote files checking the local copies
    for basefile, timestamp, container in _list_remote(store, localmaildir, verbose=verbose):
        if basefile in localfiles:
            if verbose:
                print("found %s" % basefile)
        else:
            storefile = joinpath(localstore, basefile)
            if existspath(storefile):
                if verbose:
                    print("exists %s %s" % (basefile, storefile))
            else:
                print("pulling %s %s to %s" % (basefile, container, storefile))
                stdout = store.cmd("cat %s/%s*" % (container, basefile), verbose=verbose)

                if verbose and len(stdout) < 1:
                    print("%s is an error" % storefile)

                if not noop and len(stdout) > 0:
                    with open(storefile, "w") as fd:
                        fd.write(stdout)
                    try:
                        # Now symlink the store file to the correct location
                        target = joinpath(
                            expanduser(localmaildir), 
                            basename(container),
                            basefile
                            )
                        symlink(abspath(storefile), target)
                    except OSError as e:
                        if e.errno == 17:
                            # file exists
                            pass
                        else:
                            print("%s %s %s" % (e, storefile, target))
                            
                    # If we have filters then we should pass the message object to them
                    list(_filter(stdout, mailparser, mailfilters, mdfolder))


# End
