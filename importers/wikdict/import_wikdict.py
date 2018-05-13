#!/usr/bin/env python3
#vim: set expandtab sts=4 ts=4 sw=4 autoindent ft=python:
"""This script downloads all (already approved) dictionary from the WikDict
project to be included in FreeDict's repository.
This script requires a FreeDict API file to figure out which dictionaries are
from WikDict and which not. It searches first for a local file referenced by the
FreeDict configuration. This is the preferred way. Then it downloads a version
from freedict.org.
"""

#pylint: disable=multiple-imports
import html.parser
import json
import os
import re
import sys
import urllib.request, urllib.parse
import shutil
import xml.etree.ElementTree
from datetime import date

SOURCE_URL = 'http://download.wikdict.com/dictionaries/tei/recommended/'


def get_fd_api():
    """Read local FreeDict API file or load from given path from configuration
    or download it from freedict.org as fallback."""
    js = None
    fd_tool = None # make except happy
    try:
        import fd_tool.config as config
        cnf = config.discover_and_load()
        js = json.load(open(cnf['DEFAULT']['virtual_env']))
    except fd_tool.conifg.ConfigurationError:
        pass # no conf, download json
    except ImportError:
        # try to activate virtual env, if configured but not sourced yet
        paths = [os.path.join(os.path.expanduser("~"), '.config/freedict/freedictrc')]
        if os.environ.get('LOCALAPPDATA'):
            paths.append(os.path.join(os.environ['LOCALAPPDATA'], 'freedict/freedict.ini'))
            conffile = [path for path in paths if os.path.exists(path)]
            if conffile:
                import configparser
                cnf = configparser.ConfigParser()
                cnf.read_file(open(conffile[0]))
                if 'DEFAULT' in cnf and 'api_output_path' in cnf['DEFAULT']:
                    js = json.load(open(cnf['DEFAULT']['api_output_path']))
    if not js:
        from urllib.request import urlopen
        with urlopen('https://freedict.org/freedict-database.json') as u:
            js = json.loads(u.read().decode('utf-8'))
    return js


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


def download_to(link, target):
    """Download a link to a specified (file system) target."""
    try:
        with urllib.request.urlopen(link) as u:
            open(target, 'wb').write(u.read())
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
        print("Error: must be run from a FreeDict source root.")
        sys.exit(9)


def make_changelog(path):
    tmpl_vars = {
            'date': date.today(),
            'dict': path,
    }
    with open(os.path.join(path, 'ChangeLog'), 'w') as f:
        f.write("""
{date}

  * automatic import of {dict} dictionary from WikDict
        """.strip().format(**tmpl_vars) + '\n')


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


def other_dict_exists(base_name):
    try:
        with open(os.path.join(base_name, base_name + '.tei'), 'rb') as f:
            ns = '{http://www.tei-c.org/ns/1.0}'
            parser = xml.etree.ElementTree.XMLPullParser(events=['end'])
            for line in f:
                parser.feed(line)
                for (_, element) in parser.read_events():
                    if element.tag == ns + 'sourceDesc':
                        if b'wikdict' in xml.etree.ElementTree.tostring(element):
                            return False
                        else:
                            return True
    except FileNotFoundError:
        return False
    raise Exception('No sourceDesc in dictionary ' + base_name)


def main():
    assert_correct_working_directory()
    if len(sys.argv) != 2:
        print("Error, path to shared FreeDict files required as first argument.")
        sys.exit(1)
    if not os.path.exists(sys.argv[1]):
        print("Error, path does not exist",sys.argv[1])
        sys.exit(2)
    prefix = 'http://{0.netloc}{0.path}'.format(urllib.parse.urlsplit(SOURCE_URL))
    with urllib.request.urlopen(SOURCE_URL) as src:
        data = ""
        try:
            data = src.read().decode('utf-8')
        except UnicodeEncodeError:
            raise ValueError("Could not encode page with encoding UTF-8; please adjust manually")
    links = [l  for l in extract_links(data)  if l.endswith('.tei')]
    for link in links:
        if not urllib.parse.urlsplit(link)[1]: # no host in URL
            link = urllib.parse.urljoin(prefix, link)
        base_name = os.path.splitext(link.split('/')[-1])[0] # name without .tei
        if not re.match(r'\w{3}-\w{3}', base_name):
            continue
        if other_dict_exists(base_name):
            print('Manually edited dict for {} found, skip import'.format(base_name))
            continue

        print('Importing', base_name)
        update_dict_files(base_name, sys.argv[1])
        download_to(link, os.path.join(base_name, base_name + '.tei'))
        make_changelog(base_name)

if __name__ == '__main__':
    main()

