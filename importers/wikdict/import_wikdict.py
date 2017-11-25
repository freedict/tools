#!/usr/bin/env python3
#vim: set expandtab sts=4 ts=4 sw=4 autoindent ft=python:
"""This script downloads all (already approved) dictionary from the WikDict
project to be included in FreeDict's repository."""

#pylint: disable=multiple-imports
import html.parser
import os
import re
import sys
import urllib.request, urllib.parse
import shutil
from datetime import date

SOURCE_URL = 'http://download.wikdict.com/dictionaries/tei/recommended/'

white_list = [
    'fra-deu', 'deu-fra', 'dan-deu', 'deu-dan', 'deu-swe', 'swe-deu',
    'eng-fin', 'pol-eng', 'deu-spa', 'fin-eng', 'fra-spa', 'pol-spa',
]


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
    """Check, that this script is execute from the correct directory."""
    files = sum(1 for fn in os.listdir('.')
            if re.search('^[a-z]{3}-[a-z]{3}$', fn))
    if len(files) < 4: # less than four dictionaries, probably not a dictionary place
        print("Error: must be run from the FreeDict source root.")
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


def replace_dict_dir(path):
    dir_template = os.path.join(
            os.path.dirname(os.path.realpath(__file__)),
            'template'
    )
    shutil.rmtree(path, ignore_errors=True)
    shutil.copytree(dir_template, path, symlinks=True)


def main():
    assert_correct_working_directory()
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
        if base_name in white_list:
            print('Importing',base_name)
            replace_dict_dir(base_name)
            download_to(link, os.path.join(base_name, base_name + '.tei'))
            make_changelog(base_name)
        else: print("Ignoring",base_name)
    os.system('git status')

if __name__ == '__main__':
    main()
