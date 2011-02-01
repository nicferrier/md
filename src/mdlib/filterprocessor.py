#!/usr/bin/env python

RULES = """
# My filter file
#
# Whitepsace lines and lines starting with # are ignored
#
# Filters go: regex field command 
# the filterline must begin with a non-whitespace character that is
# used as the field separator
# 
#  /Laura Chaffee/from/rm
#  /host[0-9]{2}\.tin\.woome/subject/mv woome.robot 
#
# Here we see the field separator being changed
#  |/somepath|subject|rm
#
"""

import re

class Rule(object):
    def __init__(self, pattern, field, command):
        self.pattern = pattern
        self.field = field
        self.command = command

    def __call__(self, msg):
        """Test the message against the rule and return self if it passes"""
        # I think this should optionally take an mdfolder and filter
        # the message appropriately if it's there
        if self.field:
            field = msg[self.field]
            if field:
                try:
                    match = re.search(self.pattern, field)
                    if match:
                        return self
                except Exception,e:
                    print "whoops! %s %s %s" % (self.pattern, field, self.command)
                    print e
            else:
                print "message had no %s" % self.field

    def __str__(self):
        return "%s = {%s} then %s" % (
            self.field, 
            self.pattern,
            self.command
            )

def _parse_iter(fd):
    rules = [r for r in fd if re.match("^[^# \t\n]", r)]
    for rule in rules:
        splitchar = rule[0]
        pattern, field, command = rule[1:].split(splitchar)
        yield Rule(pattern, field, command.strip())
    return

def parse(fd):
    ruleslist = list(_parse_iter(fd))
    return ruleslist

if __name__ == "__main__":
    print "nothing yet"
    
# End
