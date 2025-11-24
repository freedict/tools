#!/usr/bin/env python3
#vim: set expandtab sts=4 ts=4 sw=4 autoindent ft=python:
"""This script downloads all (already approved) dictionary from the WikDict
project to be included in FreeDict's repository.
This script requires a FreeDict API file to figure out which dictionaries are
from WikDict and which not. It searches first for a local file referenced by the
FreeDict configuration. This is the preferred way. Then it downloads a version
from freedict.org.
"""

import argparse
import enum
import html.parser
import json
import multiprocessing
import os
import re
import sys
import urllib.request, urllib.parse
import shutil
import xml.etree.ElementTree as ET
from datetime import date

SOURCE_URL = 'https://download.wikdict.com/dictionaries/tei/no-infl/'
# minimal number of words to consider a dictionary for inclusion
MIN_WORD_COUNT = 10000
DOWNLOAD_PREFIX = 'http://{0.netloc}{0.path}'.format(urllib.parse.urlsplit(SOURCE_URL))

def get_fd_api():
    """Read local FreeDict API file or load from given path from configuration
    or download it from freedict.org as fallback.
    It is advised to generate a fresh API copy locally.
    This function will try to load a virtual environment, if configured."""
    js = None
    #pylint: disable=bare-except
    api_file = lambda c: os.path.join(os.path.expanduser(
            c['DEFAULT']['api_output_path']), 'freedict-database.json')

    try:
        import fd_tool.config as config
        cnf = config.discover_and_load()
        js = json.load(open(api_file(cnf), encoding="UTF-8"))
    except ImportError:
        # try to activate virtual env, if configured but not sourced yet
        paths = [os.path.join(os.path.expanduser("~"), '.config/freedict/freedictrc')]
        if os.environ.get('LOCALAPPDATA'):
            paths.append(os.path.join(os.environ['LOCALAPPDATA'], 'freedict/freedict.ini'))
            conffile = [path for path in paths if os.path.exists(path)]
            if conffile:
                import configparser
                cnf = configparser.ConfigParser()
                cnf.read_file(open(conffile[0], encoding="UTF-8"))
                if 'DEFAULT' in cnf and 'api_output_path' in cnf['DEFAULT']:
                    with open(api_file(cnf), encoding="UTF-8") as fhandle:
                        js = json.load(fhandle)
    except KeyboardInterrupt:
        pass
    if not js:
        from urllib.request import urlopen
        with urlopen('https://freedict.org/freedict-database.json') as u:
            js = json.loads(u.read().decode('utf-8'))
    # strip all non-dictionary nodes (e.g. 'software'
    return [node for node in js if 'name' in node]


class LinkExtractor(html.parser.HTMLParser):
    """Extract all URL's from a HTML file. Use extract_links for a more
    easy-to-use function."""
    def __init__(self):
        self.links = []
        super().__init__()

    def handle_starttag(self, tag, attrs):
        if tag == 'a':
            attrs = dict(attrs)
            if 'href' in attrs:
                self.links.append(attrs['href'])


def extract_links(from_string):
    """Return a list with all links contained in the HTML page passed as input
    parameter."""
    parser = LinkExtractor()
    parser.feed(from_string)
    return parser.links


def download(link):
    """Download given link and decode the bytes."""
    try:
        with urllib.request.urlopen(link) as u:
            return u.read().decode('UTF-8')
    except urllib.error.HTTPError as h:
        if int(h.code) == 404:
            reason = '%s; url: %s' % (str(h), link)
            raise urllib.error.URLError(reason)
        else:
            raise h from None


def assert_correct_working_directory():
    """Check that this script is executed from the correct directory."""
    num_files = sum(1 for fn in os.listdir('.')
            if re.search('^[a-z]{3}-[a-z]{3}$', fn))
    if num_files < 2: # less than two dictionaries, probably not a dictionary root
        print("Error: must be run from a FreeDict source root, i.e. "
                "where all generated dictionaries are stored.")
        sys.exit(9)


def make_changelog(path):
    tmpl_vars = {
            'date': date.today(),
            'dict': path,
    }
    changelog_path = 'ChangeLog'
    changelog = ''
    if os.path.exists(changelog_path):
        with open(os.path.join(path, changelog_path), 'r', encoding="UTF-8") as chlog_file:
            changelog += chlog_file.read()
    with open(os.path.join(path, 'ChangeLog'), 'w', encoding="UTF-8") as f:
        f.write(("""\n
{date}

  * automatic import of {dict} dictionary from WikDict
        \n""" + changelog).strip().format(**tmpl_vars) + '\n')


def update_dict_files(path, shared_file_path):
    dir_template = os.path.join(
            os.path.dirname(os.path.realpath(__file__)),
            'template'
    )
    shutil.rmtree(path, ignore_errors=True)
    os.makedirs(path, exist_ok=True)
    def copy(files):
        for file in files:
            try:
                shutil.copy2(file, path)
            except shutil.SameFileError:
                pass
    copy(os.path.join(shared_file_path, f) for f in
            ('freedict-dictionary.css', 'freedict-P5.dtd', 'INSTALL',
                'freedict-P5.rng', 'freedict-P5.xml'))
    copy(os.path.join(dir_template, f) for f in os.listdir(dir_template))


def dict_exists_from_other_source(api_dump, dictname):
    """FreeDict dictionaries come from different sources. At the time of writing,
these are the VCS-tracked, partly hand-written dictionaries and the
auto-imported dictionaries. WikDict are auto-imported and hence in the bucked of
non-VCS tracked, generated dictionaries. Before importing a WikDict dictionary,
we have to check whether a dictionary with the same name exists somewhere
else."""
    dictionary = [d for d in api_dump if d['name'] == dictname]

    return dictionary and ('sourceURL' not in dictionary[0] or \
            'wikdict' not in dictionary[0]['sourceURL'])

def enough_headwords(tei):
    """This function parses the TEI header and returns true if the given
    dictionary contains more than the minimal required number of headwords."""
    tei = ET.fromstring(tei)
    node = tei.find('*//{http://www.tei-c.org/ns/1.0}extent')
    count = re.search(r'(\d+\s*,?\.?\d*)\s+.*word', node.text).groups()[0]
    return int(count.strip(' ,.')) >= MIN_WORD_COUNT

def parse_links():
    with urllib.request.urlopen(SOURCE_URL) as src:
        try:
            data = src.read().decode('utf-8')
        except UnicodeEncodeError:
            raise ValueError("Could not encode page with encoding UTF-8; please adjust manually")
        return (l  for l in extract_links(data)  if l.endswith('.tei'))

class DictionaryStrategy(enum.Enum):
    TooSmall = 0
    ManuallyEdited = 1
    Imported = 2
    Rubbish = 3

def import_dictionary(api, link, shared_dir, force_import: bool):
    """Import a dictionary from WikDict and prepare dictionary directory for a
    release."""
    if not urllib.parse.urlsplit(link)[1]: # no host in URL
        link = urllib.parse.urljoin(DOWNLOAD_PREFIX, link)
    base_name = os.path.splitext(link.split('/')[-1])[0] # name without .tei
    if not re.match(r'\w{3}-\w{3}', base_name):
        return (DictionaryStrategy.Rubbish, None)
    if not force_import and dict_exists_from_other_source(api, base_name):
        return (base_name, DictionaryStrategy.ManuallyEdited)
    tei = download(link)
    if not enough_headwords(tei):
        return (base_name, DictionaryStrategy.TooSmall)

    print('Importing', base_name)
    if not os.path.exists(base_name):
        os.makedirs(base_name)
    # erase old files, write new ones
    update_dict_files(base_name, shared_dir)
    with open(os.path.join(base_name, base_name + '.tei'), 'w',
            encoding='utf-8') as file:
        file.write(tei)
    make_changelog(base_name)
    return (base_name, DictionaryStrategy.Imported)


def main():
    assert_correct_working_directory()
    parser = argparse.ArgumentParser(allow_abbrev=False)
    parser.add_argument("-f", "--force-import", dest="force_import",
            help=("switch modes to import the given WikDict dictionary, "
                "independent of whether something is listed in the API"))
    parser.add_argument('shared_fd_dir', type=str,
            help="path to the shared FD files")
    args = parser.parse_args()
    if not os.path.exists(args.shared_fd_dir):
        print("Error, path does not exist", args.shared_fd_dir)
        sys.exit(2)
    # collect statistics
    too_small, manual = [], []
    api = get_fd_api()
    wikdict_file_listing = parse_links()
    if args.force_import is not None:
        wikdict_file_listing = [d for d in wikdict_file_listing
                if args.force_import in d]
    force_import = args.force_import is not None
    with multiprocessing.Pool(5) as p:
        res = p.starmap(import_dictionary, # ↓ pair with api, see import_dictionary
                ((api, l, args.shared_fd_dir, force_import)
                for l in sorted(wikdict_file_listing)))
        for dictname, action in res:
            if action is DictionaryStrategy.TooSmall:
                too_small.append(dictname)
            elif action == DictionaryStrategy.ManuallyEdited:
                manual.append(dictname)
    print("The following dictionaries were skipped because:")
    if manual:
        print("… a non-WikDict version exists:",', '.join(manual))
    if too_small:
        print("… they were too small:", ', '.join(too_small))


if __name__ == '__main__':
    main()

