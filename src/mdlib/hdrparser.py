import mmap

from email.parser import FeedParser
from email.parser import HeaderParser

class HeaderOnlyParser(HeaderParser):
    """JUST parse the header

    Python's HeaderParser actually parses the whole message. Duh."""

    def parse(self, fp, headersonly=True):
        """Create a message structure from the data in a file."""
        feedparser = FeedParser(self._class)
        feedparser._set_headersonly()

        try:
            mp = mmap.mmap(fp.fileno(), 0, access=mmap.ACCESS_READ)
        except:
            mp = fp

        data = ""
        while True:
            line = mp.readline()
            data = data + line
            if line == "\n":
                break
        feedparser.feed(data) # mp[0:5000])
        return feedparser.close()

# End
