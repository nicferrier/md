
from mdlib import MdFolder
from mdlib import MdClient
from mdlib.api import MDMSG_FILENAME_PATTERN
from mdlib.api import SEPERATOR
from io import StringIO

TESTMSG = """Return-Path: <%s>
X-Original-To: nic@ferrier.me.uk
Delivered-To: nic@ferrier.me.uk
Received: from mail.python.org (mail.python.org [82.94.164.166])
	by northpole.ferrier.me.uk (Postfix) with ESMTPS id A4C63E8C371
	for <nic@ferrier.me.uk>; Mon,  3 May 2010 05:16:49 +0200 (CEST)
Received: from albatross.python.org (localhost [127.0.0.1])
	by mail.python.org (Postfix) with ESMTP id 0EDACC8F7
	for <nic@ferrier.me.uk>; Mon,  3 May 2010 05:37:26 +0200 (CEST)
DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/simple; d=python.org; s=200901;
	t=1272857846; bh=M2a3faKQfZmlBA5tc06oLqzt6LIQvayrTR9svKZGw5U=;
	h=Subject:From:To:Message-Id:Date;
	b=E/xUycMlJuJwWcfur0hLn4p0+PCbU6UvPzlWdDMxhggjexhzNADeZUbxOnzKJv2A9
	 K+I06ROfEMUZUaIlEjPv4YiTt7jKEFFF4fapsgsj3vVTgOrGytEWc+qh93d8ea3bXC
	 dMtsJIEjbKZm3dvv9u5DlRipsmWkp7gppT5IPs+I=
Received: from localhost (HELO mail.python.org) (127.0.0.1)
  by albatross.python.org with SMTP; 03 May 2010 05:37:26 +0200
Received: from ximinez.python.org (ximinez.python.org [82.94.164.163])
	by mail.python.org (Postfix) with ESMTP
	for <nic@ferrier.me.uk>; Mon,  3 May 2010 05:37:25 +0200 (CEST)
Subject: Some message from python
From: richard@example.com
Message-Id: <20100503033726.0EDACC8F7@mail.python.org>
Date: Mon,  3 May 2010 05:37:26 +0200 (CEST)

Hi.
"""

TESTMSG1=TESTMSG % "someone@example1.com"
TESTMSG2=TESTMSG % "someone@example2.com"

import unittest
import re

class TestMaildir(unittest.TestCase):
     def setUp(self):
          from pyproxyfs import TestFS
          folder = MdFolder(
              "testmaildir",
              base="/var/maildir",
              filesystem = TestFS({
                      "/var/maildir/testmaildir/cur/": "",
                      "/var/maildir/testmaildir/new/1270028940.V801Ie8c95dM583793.hostname": TESTMSG1,
                      "/var/maildir/testmaildir/.special/cur/": "",
                      "/var/maildir/testmaildir/.special/new/": "",
                      "/var/maildir/testmaildir/.special/new/1270028940.V801Ie8c95dM583793.hostname": TESTMSG2,
                      })
              )
          self.folder = folder

     def test_regex(self):
          """Test the regex we use to identify the parts of maildirs"""
          m = re.match(
               "/var/maildir/testmaildir/new/%s" % MDMSG_FILENAME_PATTERN,
               "/var/maildir/testmaildir/new/1270028940.V801Ie8c95dM583793.hostname:2,"
               )
          self.assertEqual(
               m.group("key"),
               "1270028940.V801Ie8c95dM583793"
               )
          m = re.match(
               "/var/maildir/testmaildir/new/%s" % MDMSG_FILENAME_PATTERN,
               "/var/maildir/testmaildir/new/1270028940.V801Ie8c95dM583793.hostname"
               )
          self.assertEqual(
               m.group("key"),
               "1270028940.V801Ie8c95dM583793"
               )


     def test_keys(self):
          keys =  list(self.folder.keys())
          self.assertEqual(
               keys,
               ['1270028940.V801Ie8c95dM583793']
               )

     def test_key(self):
         self.assertEqual(
              self.folder["1270028940.V801Ie8c95dM583793"].__str__(),
              'MdMessage__<testmaildir>__1270028940.V801Ie8c95dM583793'
              )
         #self.assertEqual(
         #     self.folder["1270028940"].content.split('\n')[0],
         #     'Return-Path: <someone@example1.com>'
         #     )

     def test_move(self):
          folder_list = self.folder.folders()
          target = folder_list["special"]
          msg_key = "1270028940.V801Ie8c95dM583793"
          self.folder.move(msg_key, target)
          msg = target[msg_key]
          self.assertTrue(msg)
          try:
               self.folder[msg_key]
          except KeyError:
               # We expect the key error, the key has been moved away from this folder.
               pass
          else:
               assert False, "%s should not be in this folder" % msg_key

     def test_items(self):
          lst = list(self.folder.items())
          self.assertEqual(
               [(name,msg.content.split("\n")[0]) for name,msg in lst],
               [('1270028940.V801Ie8c95dM583793', 'Return-Path: <someone@example1.com>')]
               )

     def test_values(self):
          self.assertEqual(
               list(self.folder.values())[0].date.day,
               3
               )

          self.assertEqual(
               list(self.folder.values())[0].get_from(),
               'richard@example.com'
               )

     def test_contenttype(self):
          """Gets a message and checks it's the right content type"""
          self.assertEqual(
               self.folder["1270028940.V801Ie8c95dM583793"].get_content_type(),
               'text/plain'
               )

     def test_setseen(self):
          msg = self.folder["1270028940.V801Ie8c95dM583793"]
          msg.is_seen = True
          self.assertTrue(
               self.folder["1270028940.V801Ie8c95dM583793"].is_seen
               )

     def test_settrashed(self):
         self.folder["1270028940.V801Ie8c95dM583793"].is_trashed = True
         self.assertTrue(
              self.folder["1270028940.V801Ie8c95dM583793"].is_trashed
              )

     def test_remove(self):
          del self.folder["1270028940.V801Ie8c95dM583793"]
          self.assertEqual(list(self.folder.keys()), [])

     def test_folders(self):
         self.assertEqual(
              list(self.folder.folders().__iter__())[0].folder,
              'special'
              )
         self.assertEqual(
              self.folder.folders()["special"].folder,
              'special'
              )

     def test_folder_access(self):
          self.assertEqual(
               self.folder.folders()["special"]["1270028940.V801Ie8c95dM583793"].content.split('\n')[0],
               'Return-Path: <someone@example2.com>'
               )

class TestClient(unittest.TestCase):
     def setUp(self):
         from pyproxyfs import TestFS
         client = MdClient(
             "/var/maildir/testmaildir",
             filesystem = TestFS({
                     "/var/maildir/testmaildir/cur/": "",
                     "/var/maildir/testmaildir/new/1270028940.V801Ie8c95dM583793.hostname.domain.tld": TESTMSG1,
                     "/var/maildir/testmaildir/.special/cur/": "",
                     "/var/maildir/testmaildir/.special/new/": "",
                     "/var/maildir/testmaildir/.special/new/1270028941.V801Ie8c95dM583795.hostname": TESTMSG2,
                     })
             )
         self.client = client


     def test_lsfolders(self):
         stream = StringIO()
         self.client.lsfolders(stream=stream)
         self.assertEqual(
              stream.getvalue(),
              'special\n'
              )

     def test_ls(self):
          stream = StringIO()
          self.client.ls(stream=stream)
          self.assertEqual(
               [i.strip() for i in stream.getvalue().split('  ') if i.strip() != ''],
               ['INBOX%s1270028940.V801Ie8c95dM583793' % SEPERATOR, '2010-05-03 04:37:26', 'richard@example.com', '[]', 'Some message from python']
               )

     def test_lisp(self):
          self.maxDiff = None
          stream = StringIO()
          self.client.lisp(stream=stream)
          self.assertEqual(
               stream.getvalue(),
               '{"from": ["", "richard@example.com"], "flags": "", "key": "INBOX%s1270028940.V801Ie8c95dM583793", "date": "2010-05-03 04:37:26", "folder": "INBOX", "subject": "Some message from python"}\n'  % SEPERATOR
               )

     def test_folderls(self):
          stream = StringIO()
          self.client.ls(foldername="special", stream=stream)
          self.assertEqual(
               [i.strip() for i in stream.getvalue().split('  ') if i.strip() != ''],
               ['special%s1270028941.V801Ie8c95dM583795' % SEPERATOR, '2010-05-03 04:37:26', 'richard@example.com', '[]', 'Some message from python']
               )

     def test_move(self):
          stream = StringIO()
          self.client.move("INBOX%s1270028940.V801Ie8c95dM583793" % SEPERATOR, "special")
          self.client.ls(foldername="special", stream=stream)
          lines = stream.getvalue().split("\n")
          self.assertEqual(lines[0].split("  ")[0], 'special%s1270028940.V801Ie8c95dM583793' % SEPERATOR)
          self.assertEqual(lines[1].split("  ")[0], 'special%s1270028941.V801Ie8c95dM583795' % SEPERATOR)

     def test_msgdata(self):
          stream = StringIO()
          self.client.gettext(
               "INBOX%s1270028940.V801Ie8c95dM583793" % SEPERATOR,
               stream=stream
               )
          msgdata = stream.getvalue()
          self.assertEqual(
               msgdata.split("\n")[0],
               'return-path: <someone@example1.com>'
               )
          self.assertEqual(
               msgdata.split("\n")[-3],
               'Hi.'
               )

     def test_type(self):
         stream = StringIO()
         self.client.getstruct(
              "INBOX%s1270028940.V801Ie8c95dM583793" % SEPERATOR,
              stream=stream
              )
         self.assertEqual(
              stream.getvalue(),
              'text/plain\n'
              )

     def test_folders(self):
         stream = StringIO()
         self.client.ls(foldername="special", stream=stream)
         self.assertEqual(
              [i.strip() for i in stream.getvalue().split('  ') if i.strip() != ''],
              ['special%s1270028941.V801Ie8c95dM583795' % SEPERATOR, '2010-05-03 04:37:26', 'richard@example.com', '[]', 'Some message from python']
              )

     def test_folder_msg(self):
          stream = StringIO()
          self.client.gettext(
               "special%s1270028941.V801Ie8c95dM583795" % SEPERATOR,
               stream=stream
               )
          msgdata = stream.getvalue()
          self.assertEqual(
               msgdata.split("\n")[0],
               'return-path: <someone@example2.com>'
               )

     def test_folder_msg_rm(self):
          self.client.remove(
               "INBOX%s1270028940.V801Ie8c95dM583793" % SEPERATOR,
               )


from mdlib import filterprocessor
class TestFilter(unittest.TestCase):
     """Test that our simple filtering all works."""
     def setUp(self):
          self.rules = StringIO(filterprocessor.TEST_RULES)

     def test_parse(self):
          """Test that basic parsing works"""
          rules_list = filterprocessor.parse(self.rules)
          self.assertTrue(rules_list[-1].pattern == "/somepath", "%r" % rules_list[-1].pattern)
          self.assertTrue(rules_list[-1].field == "subject", "%r" % rules_list[-1].field)
          self.assertTrue(rules_list[-1].command == "rm", "%r" % rules_list[-1].command)


from mdlib import cli

class TestCli(unittest.TestCase):
     """Test that the cmdln cli is working."""
     def setUp(self):
          from pyproxyfs import TestFS
          stdout = StringIO()
          stderr = StringIO()
          self.extras = {
               "filesystem": TestFS({
                   "/var/maildir/testmaildir/cur/": "",
                   "/var/maildir/testmaildir/new/1270028940.V801Ie8c95dM583793.hostname.domain.tld": TESTMSG1,
                   "/var/maildir/testmaildir/.special/cur/": "",
                   "/var/maildir/testmaildir/.special/new/": "",
                   "/var/maildir/testmaildir/.special/new/1270028941.V801Ie8c95dM583795.hostname": TESTMSG2,
                   }),
               "do_exit": False,
               "stdout": stdout,
               "stderr": stderr
               }

     def test_ls(self):
          """Test we can list a folder correctly."""
          cli.main(
               ["md", "-M", "/var/maildir/testmaildir", "ls"],
               **self.extras
               )
          output = self.extras["stdout"].getvalue()
          rx = r"^(?P<ref>[^ ]+)[ \t]+" \
              + r"2010-05-03 " \
              + r"(?P<time>[0-9]{2}:[0-9]{2}:[0-9]{2})[ \t]+" \
              + r"(?P<from>[a-zA-Z0-9.@]+)[ \t]+" \
              + r"(.*)$"
          m = re.match(rx, output)
          self.assertTrue(m)
          # We can't test the time because of timezones!!! OMFG!!!
          # self.assertEqual(m.group("time"), "04:37:26")
          self.assertEqual(m.group("from"), "richard@example.com")
          self.assertEqual(m.group("ref"), "INBOX%s1270028940.V801Ie8c95dM583793" % SEPERATOR)

     def test_text(self):
          """Test we find the plain text of the message."""
          cli.main(
               ["md", "-M", "/var/maildir/testmaildir", 
                "text", "INBOX%s1270028940.V801Ie8c95dM583793" % SEPERATOR], 
               **self.extras
               )
          output = self.extras["stdout"].getvalue()
          m = re.search("Hi.", output)
          self.assertTrue(m)


if __name__ == "__main__":
     unittest.main()

# End
