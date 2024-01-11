"""This module holds datastructures to represent a dictionary and links to their
downloads and a few function to retrieve useful information about these
objects."""
import datetime
import distutils.version
import enum
import os
import re
import urllib.request

import semver

from . import config


class Dictionary:
    """Dictionary class holding various properties of a dictionary.
    This class is intended to hold all properties of a Dictionary node in the
    resulting XML API file.

    A dictionary object consists of optional and mandatory information:

    mandatory: headwords, edition, date
    optional = maintainerName, maintainerEmail, status, sourceURL

    The date should be the date of the last release.
    Additionally, a dictionary keeps a list of downloads which may be empty.
    Each download has to be a Link() object.
    """

    def __init__(self, name):
        self.__name = name
        # mandatory dictionary information
        mandatory = ["headwords", "edition", "date"]
        self.__mandatory = {f: None for f in mandatory}
        # optional dictionary info
        self.__optional = {
            f: None
            for f in ["maintainerName", "maintainerEmail", "status", "sourceURL"]
        }
        self.__downloads = []

    def get_name(self):
        """Return name of dictionary. This is a three-letter-code followed by a
        hyphen and followed by a three-letter-code.
        Example: deu-fra"""
        return self.__name

    def add_download(self, link):
        """Add a link (of type Link) to the linst of downloadss."""
        if not isinstance(link, Link):
            raise TypeError("Link must be of type Link()")
        self.__downloads.append(link)

    def get_downloads(self):
        """Return all download links."""
        return self.__downloads

    def __getitem__(self, key):
        """Transparently select a key from either optional or mandatory keys."""
        if key in self.__mandatory:
            return self.__mandatory[key]
        if key in self.__optional:
            return self.__optional[key]
        raise KeyError(key)

    def __contains__(self, key):
        try:
            self.__getitem__(key)
        except KeyError:
            return False
        else:
            return True

    def __setitem__(self, key, value):
        if key in self.__mandatory:
            self.__mandatory[key] = value
        elif key in self.__optional:
            self.__optional[key] = value
        else:
            raise KeyError(key)

    def get_mandatory_keys(self):
        """Return all mandatory keys."""
        return self.__mandatory.keys()

    def is_complete(self):
        """Return true if all mandatory fields are set, else false."""
        return not self._get_missing_keys()

    def _get_missing_keys(self):
        """Return list of keys which haven't been set yet but which are
        mandatory. Empty list means everything has been set."""
        return [k for k in self.__mandatory if self.__mandatory[k] is None]

    def update(self, other):
        """This method works like the .update method on a dictionary, but it
        raises an exception whenever an unknown key is found in the supplied
        dictionary."""
        if not hasattr(other, "__getitem__") or not hasattr(other, "keys"):
            raise TypeError("Object must provide methods keys() and __getitem__.")
        for key in other.keys():
            self[key] = other[key]

    def get_attributes(self):
        """Return all attributes which make up the dictionary node in the
        FreeDict XML. If mandatory attributes are not set, this function WILL
        NOT raise an exception, consequently, is_complete() must be called
        beforehand. Unset values are None.
        Hint: the name is not contained, use get_name() instead."""
        attributes = self.__mandatory.copy()
        attributes.update(self.__optional)
        return attributes


# only match x.x, x.x.x, x-y-z, x-z
DICTIONARY = "([a-z]{3}-[a-z]{3})"


class DownloadFormat(enum.Enum):
    """The download format, consisting both of the platform and the archive
    format. Some formats might not have a archive format, though."""

    DictTxz = re.compile(r"freedict-%s-(.*?).dictd.tar.xz" % DICTIONARY)
    Slob = re.compile(r"freedict-%s-(.*?).slob" % DICTIONARY)
    StardictTxz = re.compile(r"freedict-%s-(.*?).stardict.tar.xz" % DICTIONARY)
    Source = re.compile(
        r"freedict-%s-(.*?).src.(?:zip|tar.bz2|tar.gz|tar.xz)" % DICTIONARY
    )

    # legacy formats
    DictTgz = re.compile(r"freedict-%s-(.*?).dictd.tar.gz" % DICTIONARY)
    DictBz2 = re.compile(r"freedict-%s-(.*?).dictd.tar.bz2" % DICTIONARY)
    Dic = re.compile(r"freedict-%s-(.*?).dic.tar.*" % DICTIONARY)

    @staticmethod
    def get_type(file_name):
        """This function allows to get the correct enum value from a given
        file_name. The file name is parsed and the corresponding enum value
        returned. If the format could not be extracted, None is returned."""
        if file_name.endswith(".sha512"):
            return None
        format = DownloadFormat.Source
        for format in DownloadFormat:
            if format.value.search(file_name):
                return format
        return None

    def __str__(self):
        """Return a string representation of the enum value, as used in the type
        attribute of the release tag in the FreeDict XML API."""
        formats = {
                DownloadFormat.Source: "src",
                DownloadFormat.DictTgz: "dict-tgz",
                DownloadFormat.DictBz2: "dict-bz2",
                DownloadFormat.DictTxz: "dictd",
                DownloadFormat.Slob: "slob",
                DownloadFormat.StardictTxz: 'stardict'
            }
        try:
            return formats[self]
        except KeyError:
            raise ValueError("Unsupported format: " + self.name)


class Link:
    """Represent a (download) link.

    The link is made of of multiple parts. The hostname and base URI is taken
    from variables defined in the module config.
    The given path is assumed to exist on disk, so that the file can be
    determined.
    Additionally to the given link path, a link also saves the information about
    the file format (dict-bz2 or source) and also the version of the
    dictionary.
    It also mandates a hash for verification of the file."""

    def __init__(self, path, format, version, sha):
        self.path = path
        self.format = format
        self.version = version
        self.size = -1
        self.last_modification_date = "NONE"  # YYYY-MM-dd
        self.hash = sha

    def __str__(self):
        """Get a download link."""
        # split the path into chunks, url-quote them
        path = tuple(map(urllib.request.quote, self.path.split(os.sep)))
        if len(path) < 3:
            raise ValueError(
                "Required is a path with the structure LongName/version/filename"
            )
        path_base = config.RELEASE_HTTP_BASE
        if not path_base.endswith("/"):
            path_base += "/"
        return "https://{}{}{}/{}/{}".format(
            config.PROJECTHOME_HOST, path_base, path[-3], path[-2], path[-1]
        )


def normalize_version(vers_string):
    """Make a semantic version out of a less strict version number , e.g. 0.5 to
    0.5.0."""
    try:
        return semver.parse(vers_string)
    except ValueError:
        return
    semver.parse(".".join(distutils.version.StrictVersion(vers_string).version))


def mklink(full_path, format, version, sha):
    """Create a Link object with all the required information, i.e. file size.
    It queries the referenced `full_path` for its size and last modification
    date. It doesn't care whether it's actually on the file system or mounted
    with e.g. sshfs."""
    chunks = full_path.split(os.sep)
    path = "{}/{}/{}".format(chunks[-3], chunks[-2], chunks[-1])
    link = Link(path, format, version, sha)
    # get file size
    link.size = os.path.getsize(full_path)
    # get last modification date
    link.last_modification_date = datetime.datetime.fromtimestamp(
        os.path.getmtime(full_path)
    ).strftime("%Y-%m-%d")
    return link
