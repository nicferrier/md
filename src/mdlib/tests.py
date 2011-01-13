
from mdlib import MdFolder
from mdlib import MdClient
from mdlib import MDMSGPATHRE
from mdlib import SEPERATOR
from StringIO import StringIO

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
To: nic@ferrier.me.uk
Message-Id: <20100503033726.0EDACC8F7@mail.python.org>
Date: Mon,  3 May 2010 05:37:26 +0200 (CEST)

Hi.
"""
TESTMSG1=TESTMSG % "someone@example1.com"
TESTMSG2=TESTMSG % "someone@example2.com"


import unittest

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
          import re
          m = re.match(
               MDMSGPATHRE % "/var/maildir/testmaildir/new",
               "/var/maildir/testmaildir/new/1270028940.V801Ie8c95dM583793.hostname:2,"
               )
          self.assertEquals(
               m.group("key"),
               "1270028940.V801Ie8c95dM583793"
               )
          m = re.match(
               MDMSGPATHRE % "/var/maildir/testmaildir/new",
               "/var/maildir/testmaildir/new/1270028940.V801Ie8c95dM583793.hostname"
               )
          self.assertEquals(
               m.group("key"),
               "1270028940.V801Ie8c95dM583793"
               )


     def test_keys(self):
         self.assertEquals(
              self.folder.keys(),
              ['1270028940.V801Ie8c95dM583793']
              )

     def test_key(self):
         self.assertEquals(
              self.folder["1270028940.V801Ie8c95dM583793"].__str__(),
              'MdMessage__<testmaildir>__1270028940.V801Ie8c95dM583793'
              )
         #self.assertEquals(
         #     self.folder["1270028940"].content.split('\n')[0],
         #     'Return-Path: <someone@example1.com>'
         #     )

     def test_items(self):
          lst = self.folder.items()
          self.assertEquals(
               [(name,msg.content.split("\n")[0]) for name,msg in lst],
               [('1270028940.V801Ie8c95dM583793', 'Return-Path: <someone@example1.com>')]
               )

     def test_values(self):
          self.assertEquals(
               self.folder.values()[0].date.day,
               3
               )

          self.assertEquals(
               self.folder.values()[0].get_from(),
               'richard@example.com'
               )

     def test_contenttype(self):
          """Gets a message and checks it's the right content type"""
          self.assertEquals(
               self.folder["1270028940.V801Ie8c95dM583793"].get_content_type(),
               'text/plain'
               )

     def test_setseen(self):
          self.folder["1270028940.V801Ie8c95dM583793"].is_seen = True
          self.assert_(
               self.folder["1270028940.V801Ie8c95dM583793"].is_seen
               )

     def test_settrashed(self):
         self.folder["1270028940.V801Ie8c95dM583793"].is_trashed = True
         self.assert_(
              self.folder["1270028940.V801Ie8c95dM583793"].is_trashed
              )

     def test_remove(self):
          del self.folder["1270028940.V801Ie8c95dM583793"]
          self.assertEquals(self.folder.keys(), [])

     def test_folders(self):
         self.assertEquals(
              list(self.folder.folders().__iter__())[0],
              '.special'
              )
         self.assertEquals(
              self.folder.folders()[0].folder,
              '.special'
              )

     def test_folder_access(self):
          self.assertEquals(
               self.folder.folders()[0]["1270028940.V801Ie8c95dM583793"].content.split('\n')[0],
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
                     "/var/maildir/testmaildir/.special/new/1270028941.V801Ie8c95dM583793.hostname": TESTMSG2,
                     })
             )
         self.client = client


     def test_lsfolders(self):
         stream = StringIO()
         self.client.lsfolders(stream=stream)
         self.assertEquals(
              stream.getvalue(),
              '.special\n'
              )

     def test_ls(self):
          stream = StringIO()
          self.client.ls(stream=stream)
          self.assertEquals(
               [i.strip() for i in stream.getvalue().split('  ') if i.strip() != ''],
               ['testmaildir%s1270028940.V801Ie8c95dM583793' % SEPERATOR, '2010-05-03 04:37:26', 'richard@example.com', '[]', 'Some message from python']
               )

     def test_lisp(self):
          stream = StringIO()
          self.client.lisp(stream=stream)
          self.assertEquals(
               stream.getvalue(),
               '{"from": ["", "richard@example.com"], "flags": "", "key": "testmaildir%s1270028940.V801Ie8c95dM583793", "date": "2010-05-03 04:37:26", "folder": "testmaildir", "subject": "Some message from python"}\n'  % SEPERATOR
               )

     def test_folderls(self):
          stream = StringIO()
          self.client.ls(foldername="special", stream=stream)
          self.assertEquals(
               [i.strip() for i in stream.getvalue().split('  ') if i.strip() != ''],
               ['special%s1270028941.V801Ie8c95dM583793' % SEPERATOR, '2010-05-03 04:37:26', 'richard@example.com', '[]', 'Some message from python']
               )

     def test_msgdata(self):
          stream = StringIO()
          self.client.gettext(
               "INBOX%s1270028940.V801Ie8c95dM583793" % SEPERATOR,
               stream=stream
               )
          msgdata = stream.getvalue()
          self.assertEquals(
               msgdata.split("\n")[0],
               'return-path: <someone@example1.com>'
               )
          self.assertEquals(
               msgdata.split("\n")[-3],
               'Hi.'
               )

     def test_type(self):
         stream = StringIO()
         self.client.getstruct(
              "INBOX%s1270028940.V801Ie8c95dM583793" % SEPERATOR,
              stream=stream
              )
         self.assertEquals(
              stream.getvalue(),
              'text/plain\n'
              )

     def test_folders(self):
         stream = StringIO()
         self.client.ls(foldername="special", stream=stream)
         self.assertEquals(
              [i.strip() for i in stream.getvalue().split('  ') if i.strip() != ''],
              ['special%s1270028941.V801Ie8c95dM583793' % SEPERATOR, '2010-05-03 04:37:26', 'richard@example.com', '[]', 'Some message from python']
              )

     def test_folder_msg(self):
          stream = StringIO()
          self.client.gettext(
               "special%s1270028941.V801Ie8c95dM583793" % SEPERATOR,
               stream=stream
               )
          msgdata = stream.getvalue()
          self.assertEquals(
               msgdata.split("\n")[0],
               'return-path: <someone@example2.com>'
               )

     def test_folder_msg_rm(self):
          self.client.remove(
               "INBOX%s1270028940.V801Ie8c95dM583793" % SEPERATOR,
               )

          

if __name__ == "__main__":
     unittest.main()

# End
