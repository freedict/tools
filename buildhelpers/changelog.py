#!/usr/bin/env python3
"""Releasing a bunch of dictionaries can be tedious because of the change log
editing, copyright adjusting and more. This script helps to automate this
process as much as possible.
Please see the usage from parse_args() for more details."""

# To Do: the classes in this file are a *fork* of rm_duplicates.py. We
# ought to find a way to properly ship this as a module.

import datetime
import os
import re
import shutil
import sys
import xml.etree.ElementTree as ET

def get_editor():
    """Detect an editor to use. Try to use $EDITOR or probe for a bunch of
    more common ones."""
    editor = None
    if 'EDITOR' in os.environ:
        editor = os.environ['EDITOR']
    if not editor:
        for name in ('vim', 'vi', 'nvim', 'emacs', 'nano', 'edit', 'notepad',
                'gedit', 'pluma', 'kate'): # a few more popular ones
            if shutil.which(name):
                editor = name
                break
    if not editor:
        print(("Failed to detect an editor. Please set the EDITOR environment "
                "variable to a program in your PATH."))
        sys.exit(1)
    else:
        return editor



class TagNotFoundException(Exception):
    pass

def find_tag(document, tag):
    """Find a tag in a document, returning start and end positions of the
    opening and closing tag."""
    match = re.search(r'<\s*%s\b.*?>' % tag, document)
    if not match:
        raise TagNotFoundException(tag)
    opening_start, opening_end = match.span()
    # find closing tag
    match = re.search(r'<\s*/\s*%s\s*>' % tag, document[opening_end:])
    if not match:
        raise TagNotFoundException('no closing tag for `%s`' % tag)
    closing_start, closing_end = (c + opening_end for c in match.span())
    # python's ranges are exclusive, so add + 1
    return (opening_start, opening_end, closing_start, closing_end)

def get_text(document, tag):
    """Get the text of a specified tag."""
    _, start, end, _ = find_tag(document, tag)
    return document[start:end]

def replace_tag_content(document, tag, new_text):
    _, start, end, _ = find_tag(document, tag)
    return document[:start] + new_text + document[end:]

def add_changelog_entry(document, edition, date, username, author=None):
    """Try to detect an text editor, open it and add the written content to a
    new change tag within the supplied revision_desc."""
    editor = get_editor()
    fn = 'changelog.tmp'
    with open(fn, 'w', encoding='UTF-8') as f:
        f.write("""\n
# Please enter your change notes as you would enter them in a </change> tag in
# the revisionDesc tag of a TEI header. Empty lines will be ignored and lines
# starting with a hash `#` will be ignored.
# Valid formatting include either plain text or lists like
# `<list><item>blah</item></list>`. Please note that you need to take care of
# escaping yourself.
""")
    ret = os.system('%s %s' % (editor, fn))
    if ret:
        print("Error while starting", editor)
        sys.exit(2)
    with open(fn, encoding='UTF-8') as f:
        data = '\n'.join(l.rstrip() for l in f
                if l.strip() and not l.lstrip().startswith('#')) + '\n'
    os.remove(fn)
    change = '<change when="{}" who="{}" n="{}">\n'.format(date,
            username, edition)
    if author:
        change += '<name>%s</name>' % author
    change += '%s\n</change>' % data
    latest_change, _, _, _ = find_tag(document, 'change')
    latest_change_tag = latest_change
    latest_change -= 1
    while latest_change > 0 and document[latest_change].isspace() and \
            document[latest_change] != '\n':
        latest_change -= 1
    indent = document[latest_change+1:latest_change_tag]
    change = change.rstrip().replace('\n', '\n' + indent)
    return ''.join((document[:latest_change],
                '\n', indent, change, '\n', indent,
                document[latest_change_tag:])).lstrip()

def update_date(document, date):
    """Find publicationStmt/date, update it."""
    opening_start, _, _, closing_end = find_tag(document, 'date')
    if opening_start < 0:
        return document # no date, no action
    # try to detect whether this date is within a change tag
    change = document[:opening_start].rfind('<change')
    if change > 0:
        return document
    date = datetime.datetime.now().strftime('<date when="%Y-%m-%d">%b %d, %Y</date>')
    return document[:opening_start] + date + document[closing_end:]

def update_edition(document, version):
    return replace_tag_content(document, 'edition', version)

def update_extent(document):
    headwordcount = len(re.findall(r'<\s*entry.*?>', document))
    return replace_tag_content(document, 'extent', '%s headwords' % headwordcount)


def update_copyright(document):
    """Find a stanza containing "(c) 2014-2017 xyz" or "© 2020 foo" to update
    the year."""
    availability = get_text(document, 'availability')
    match = re.search(r'(©|\([cC]\))\s*([0-9]{4})(?:-)([0-9]{4})', availability)
    if not match:
        return document
    start, end = match.span()
    year = match.groups()[1]
    if match.groups()[2]:
        year = match.groups()[2]
    availability = availability[:start] + availability[start:end].replace(year,
                    datetime.datetime.now().strftime('%Y')) + \
                availability[end:]
    return replace_tag_content(document, 'availability', availability)

def parse_args():
    def usage(msg=None):
        if msg:
            print(msg)
        print("Usage: make E=<EDITION>")
        print("   or: %s <EDITION> <INPUT_FILE>" % \
                os.path.basename(sys.argv[0]))
        print("\nThis script assists in changing a TEI header for release. It does the following:")
        print("- update the edition")
        print("- updates the release date")
        print("- adds a new changelog entry")
        print("- update the year in the availability / copyright section, if appropriate")
        print("\nExample: make E=1.5.2 lat-deu.tei")
        sys.exit(0)
    if len(sys.argv) == 2 and sys.argv[1] in ('-h', '--help'):
        usage()
    elif len(sys.argv) != 3:
        usage("The arguments edition and input file are mandatory.")
    else:
        return sys.argv[1:3]

def main():
    edition, input_file = parse_args()
    # register TEI name space without prefix to dump the *same* XML file
    ET.register_namespace('', 'http://www.tei-c.org/ns/1.0')
    # ToDo: username, author
    isodate = datetime.datetime.today().strftime('%Y-%m-%d')
    username = 'humenda'
    with open(input_file, 'r', encoding='UTF-8') as f:
        document = f.read()
    document = add_changelog_entry(document, edition, isodate, username)
    document = update_date(document, isodate)
    document = update_copyright(document)
    document = update_extent(document)
    document = update_edition(document, edition)
    with open(input_file, 'w', encoding='UTF-8') as f:
        f.write(document)

main()
