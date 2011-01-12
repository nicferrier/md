#!/usr/bin/env python

"""Use ssh to retrieve remote maildir files.

This defines the flat local store for ensuring that we can synchronize
a changing remote end with a changing local end. All the actual
maildir files are symlinks into the store.

The store needs abstraction so it's not tied just to this ssh
method. It also needs maintenance commands to flush back local changes
to the upstream maildir. For example removing files on the upstream
that have no links into the maildir structure locally.

The protocol is implemented with basic gnu commands 'find', 'sed' and
'cat'
"""

from subprocess import Popen
from subprocess import PIPE

class _SSH(object):
    """A very simple ssh client based on the unix command.

    This could easily be made better using one of the python ssh
    implementations such as paramiko."""

    def __init__(self, host):
        self.host = host

    def cmd(self, cmd):
        p = Popen([
                "ssh",
                "-T",
                self.host,
                cmd
                ], stdin=PIPE, stdout=PIPE, stderr=PIPE)
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

def pull(host, maildir, localmaildir, noop=False, verbose=False):
    localstore = expanduser(joinpath(localmaildir, "store"))
    
    # Get the list of mail we already have locally
    maildir_pattern = re.compile("^([0-9]+\\.[A-Za-z0-9]+)(\\.([.A-Za-z0-9-]+))*(:[2],([PRSTDF]*))*(.*)")
    localfiles = [
        maildir_pattern.match(f).group(1) 
        for f in listdir(localstore) if maildir_pattern.match(f)
        ]

    # Make the ssh connection
    np = _SSH(host)

    # This command produces a list of all files in the maildir like:
    #   base-filename timestamp container-directory
    command = """echo {maildir}/{{cur,new}} | tr ' ' '\\n' | while read path ; do ls -1Ugo --time-style=+%s $path | sed -rne "s|[a-zA-Z-]+[ \t]+[0-9]+[ \t]+[0-9]+[ \t]+([0-9]+)[ \t]+([0-9]+\\.[A-Za-z0-9]+)(\\.([.A-Za-z0-9-]+))*(:[2],([PRSTDF]*))*|\\2 \\1 $path|p";done""".format(
        maildir=maildir
        )
    if verbose:
        print command
    stdout = np.cmd(command)
    lines = stdout.split("\n")
    maildir_ls = [line.split(" ") for line in lines if len(line.split(" ")) == 3]

    # If we get problems with not finding files in the local list it can help to dump the local list
    #with open("/tmp/mdlog", "w") as fd:
    #    print >>fd, "\n".join(localfiles)
        
    # Loop through the remote files checking the local copies
    for basefile, timestamp, container in maildir_ls:
        if basefile in localfiles:
            if verbose:
                print "found %s" % basefile
        else:
            storefile = joinpath(localstore, basefile)
            if existspath(storefile):
                if verbose:
                    print "exists %s %s" % (basefile, storefile)
            else:
                print "pulling %s %s to %s" % (basefile, container, storefile)
                stdout = np.cmd("cat %s/%s*" % (container, basefile))
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
                    except OSError, e:
                        if e.errno == 17:
                            # file exists
                            pass
                        else:
                            print "%s %s %s" % (e, storefile, target)

# End
