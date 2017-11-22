#!/usr/bin/env python3
"""This script downloads all (already approved) dictionary from the WikDic
project to be included in FreeDict's repository."""

#pylint: disable=multiple-imports
import html.parser
import os
import sys
import urllib.request, urllib.parse

SOURCE_URL = 'http://download.wikdict.com/dictionaries/tei/basic/'

white_list = ['fra-deu', 'deu-fra', 'dan-deu', 'deu-dan', 'deu-swe', 'swe-deu']


class LinkExtractor(html.parser.HTMLParser):
    """Link extractor class."""
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
    """Download a link to a specified target."""
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
    import re
    files = filter(bool, [re.search('^[a-z]{3}-[a-z]{3}$', fn) for fn in
        os.listdir('.')])
    if len(tuple(files)) < 4: # well, possibly smaller, but add some fuzzyness
        print("Error: must be run from the FreeDict source root.")
        sys.exit(9)


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
            # check that target directory exists
            if not os.path.exists(base_name):
                raise OSError("Couldn't find directory for '%s', needs to be created first." % base_name)
            download_to(link, os.path.join(base_name, base_name + '.tei'))
        else: print("Ignoring",base_name)
    os.system('git status')

main()
#vim: set expandtab sts=4 ts=4 sw=4 autoindent ft=xml:
