"""``parsers'' fetching all information about downloadable releases belong in
here."""
import distutils.version
import os
import re
import shutil
import subprocess
import sys

import semver

from .config import RELEASE_HTTP_TOOL_BASE
from .dictionary import DownloadFormat


class ReleaseError(Exception):
    """This error can occur, when information about a release is gathered. It
    wraps all sorts of ValueErrors and IoErrors."""


def git(cmd):
    proc = subprocess.Popen(['git'] + cmd, cwd=os.environ['FREEDICT_TOOLS'],
            stdout=subprocess.PIPE)
    stdout = proc.communicate()[0].strip()
    ret = proc.wait()
    if ret:
        raise ReleaseError("`git %s` exited with exit code %d" % \
                (' '.join(cmd), ret))
    return stdout.decode(sys.getdefaultencoding())

def get_tools_release():
    """Retrieve the latest FreeDicttools release as a tuple with containing
    (version, date, downloadlink)."""
    if not 'FREEDICT_TOOLS' in os.environ or not shutil.which('git'):
        raise ReleaseError(("Unable to retrieve list of rleases of "
            "FreeDict tools. Either FREEDICT_TOOLS is unset or git not "
            "installed."))
    releases = git(['tag']).split('\n')
    max_ver = '0.0.0'
    for tag in releases:
        max_ver = semver.max_ver(max_ver, tag)
    if max_ver == '0.0.0':
        raise ReleaseError("No tools releases found.")
    date = re.search('^([0-9]{4}-[0-9]{2}-[0-9]{2})',
            git(['show', '-s', '--pretty=format:%ci'])).groups()[0]
    return (max_ver, date, '{}/freedict-tools-{}.tar.xz'.format(
            RELEASE_HTTP_TOOL_BASE, max_ver))

def get_release_info_for_dict(path, version):
    """Retrieve information about the releases of a dictionary."""
    files = {}
    version = None
    name = None
    for file in os.listdir(path):
        format = DownloadFormat.get_type(file)
        if not format:
            continue # ignore unknown file naming, possibly outdated or unsupported formats

        parsed_name, parsed_version = format.value.search(file).groups()

        if not version:
            parsed_version = version
        elif parsed_version != version:
            raise ReleaseError('Version from file "%s" did not match version of directory "%s"' \
                    % (file, version))
        if not name:
            name = parsed_name
        elif parsed_name != name:
            raise ReleaseError('Found two different dictionary identifiers in %s: %s and %s' \
                        % (path, parsed_name, name))
        full_path = os.path.join(path, file)
        try:
            with open(full_path + '.sha512', 'r') as f:
                sha = f.read().strip().split('  ')[0]
        except FileNotFoundError:
            raise FileNotFoundError('expected a sha512 checksum, found nothing',
                    full_path + '.sha512', 'r')
        files[full_path] = (name, format, sha)
    if not files:
        raise ReleaseError("no downloads available for %s" % path)
    return files


def get_all_downloads(root):
    """Get all paths to a downloadable file from a given root.
    This function walks the given path and returns all paths to files with the
    relative to the given root."""
    # only match directories with two iso 639-3 codes, separated by "-"
    dirpattern = re.compile(r'[a-z]{3}-[a-z]{3}')
    release_directories = tuple(os.path.join(root, e) for e in os.listdir(root)
            if os.path.isdir(os.path.join(root, e)) and dirpattern.search(e) \
                    and not 'tools' in e.lower())
    if not release_directories:
        raise ReleaseError("%s: no released dictionary found" % root)

    dictionaries_with_release = {} # collect dictionaries which have a release
    for dictdir in release_directories:
        # iterate over subdirectories which represent versions
        for version_dir in (os.path.join(dictdir, f) for f in os.listdir(dictdir)):
            version = os.path.basename(version_dir)

            try:
                files = get_release_info_for_dict(version_dir, version)
            except ReleaseError as e:
                if e.args[0].startswith('no downloads available'):
                    continue # skip versions with no or unknown releases
                else:
                    raise
            if not files:
                print('Warning: no files in "%s", skipping...' % version)
                continue
            name = next(iter(files.values()))[0]
            if not name in dictionaries_with_release:
                dictionaries_with_release[name] = {}
            # transform {path: (name, format, hash)} to (path, format, hash)
            dictionaries_with_release[name][version] = tuple((k, v[1], v[2])
                    for k,v in files.items())
    return dictionaries_with_release


def get_latest_version(release_information):
    """Iterate over object and return the latest version, as defined by
    distutils.version.StrictVersion. The argument is intended to be a dictionary
    with versions as key, but anything outputing version strings will work."""
    latest = None
    latest_strict = None # might contain '-' replaced through '.'
    for version in release_information:
        version_strict = version[:]
        if '-' in version_strict:
            version_strict = version_strict.replace('-', '.')
        try:
            version_strict = distutils.version.StrictVersion(version_strict)
        except ValueError as e:
            raise ReleaseError(e.args)
        if not latest:
            latest = version
            latest_strict = version_strict
        else:
            if version_strict > latest_strict:
                latest = version
                latest_strict = version_strict
    if not latest:
        raise ReleaseError("No versions found for " % repr(release_information))
    return latest
