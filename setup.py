#from setuptools import setup
#from setuptools import find_packages
from setuptools import setup


# Get the version
import os.path
import sys
sys.path = [os.path.join(os.path.dirname(__file__), "src")] + sys.path
from mdlib import __version__ as md_version


classifiers = [
    'Development Status :: 3 - Alpha',
    'Intended Audience :: Developers',
    'License :: OSI Approved :: GNU General Public License (GPL)',
    'Operating System :: OS Independent',
    'Programming Language :: Python',
    'Topic :: Utilities',
    'Topic :: Communications :: Email',
]

# Depends
# pyproxyfs
# cmdln
setup(
    name = "md",
    version = md_version,
    description = "Maildir command line mail client and library",
    long_description = """A command line tool and shell to allow use of
 a maildir directly. Fully fledged MUAs should be trivial to build on top of this.""",
    license = "GNU GPL v3",
    author = "Nic Ferrier",
    author_email = "nic@ferrier.me.uk",
    url = "http://github.com/nicferrier/md",
    download_url="http://github.com/nicferrier/md/downloads",
    platforms = ["unix"],
    packages = ["mdlib"],
    package_dir = {"":"src"},
    install_requires=['pyprofyfs'],
    test_suite = "mdlib",
    entry_points = {
       'console_scripts': [
            'md = mdlib.cli:main'
        ],
    },
    classifiers =  classifiers
    )
