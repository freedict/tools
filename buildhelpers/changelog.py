#!/usr/bin/env python3
"""Releasing a bunch of dictionaries can be tedious because of the change log
editing, copyright adjusting and more. This script helps to automate this
process as much as possible.
Please see the usage from parse_args() for more details."""

# To Do: the classes in this file are a *fork* of rm_duplicates.py. We
# ought to find a way to properly ship this as a module.

import datetime
import io
import os
import re
import shutil
import sys
import xml.etree.ElementTree as ET

# TEI name space, LXML's parser doesn't handle them nicely
TEI_NS = '{http://www.tei-c.org/ns/1.0}'
# findall/iter with TEI namespace removed
findall = lambda x,y: x.findall(TEI_NS + y)
tei_iter = lambda x,y: x.iter(TEI_NS + y)


class CommentedTreeBuilder(ET.TreeBuilder):
    """A TreeBuilder subclass that retains XML comments from the source. It can
    be treated as a black box saving the contents before and after the root tag,
    so that it can be re-added when writing back a XML ElementTree to disk. This
    is necessary because of lxml/ElementTree's inability to handle declarations
    nicely."""
    def comment(self, data):
        self.start(ET.Comment, {})
        self.data(data)
        self.end(ET.Comment)



#pylint: disable=too-few-public-methods
class XmlParserWrapper:
    """This thin wrapper guards the parsing process.  It manually finds the TEI
    element and copies everything before and afterwards *verbatim*. This is due
    to the inability of the ElementTree parser to handle multiple "root
    elements", for instance comments before or after the root node or '<!'
    declarations.
    """
    def __init__(self, file_name):
        with open(file_name, encoding='utf-8') as file:
            content = file.read()
        if not any(u in content for u in ('utf-8', 'utf8', 'UTF8', 'UTF-8')):
            raise ValueError("XML file is not encoded in UTF-8. Please recode "
                    "the file or extend this parser and XML writer.")
        tei_start = content.find('<TEI')
        if tei_start < 0:
            raise ValueError("Couldn't find string `<TEI` in the XML file.  Please extend this parser.")
        self.before_root = content[:tei_start]
        content = content[tei_start:]
        tei_end = content.find('</TEI>')
        if tei_end < 0:
            raise ValueError("Couldn't find `</TEI>` in the input file, please extend the parser.")
        tei_end += len('</TEI>')
        self.after_root = content[tei_end:]
        content = content[:tei_end]
        parser = ET.XMLParser(target = CommentedTreeBuilder())
        parser.feed(content)
        self.root = parser.close()

    def write(self, file_name):
        """Write the XML element tree to a file, with hopefully a very similar
        formatting as before."""
        tree = ET.ElementTree(self.root)
        in_mem = io.BytesIO()
        tree.write(in_mem, encoding="UTF-8")
        in_mem.seek(0)
        with open(file_name, 'wb') as file:
            file.write(self.before_root.encode('UTF-8'))
            file.write(in_mem.read())
            file.write(self.after_root.encode('UTF-8'))
            if not self.after_root.endswith('\n'):
                file.write(b'\n')

def get_spacing(element):
    """This function returns a tuple with the mnemonics (linebreak, indentation)
    of this element. If a the node is indented with four spaces, the tuple will
    be ('\\n', '    ') and if there's no whitespace between the nodes, it'll be
    ('', '')."""
    spaces = re.search('^\\n*( +|\\t+)', element.text)
    return (('\n' if element.text.startswith('\n') else ''),
            (spaces.groups()[0] if spaces else ''))

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


def add_changelog_entry(revision_desc, edition, date, username, author=None):
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
    change = ET.Element('change')
    change.attrib['when'] = date
    change.attrib['who'] = username
    change.attrib['n'] = edition
    if author:
        author_node = ET.Element('name')
        author_node.text = author
        change.append(author)
    lbreak, spaces = get_spacing(revision_desc)
    try:
        child = ET.fromstring(data)
        change.text = lbreak + spaces
        change.append(child)
    except ET.ParseError: # treat it as plain text
        change.text = data + spaces
    change.tail = lbreak + spaces
    revision_desc.insert(0, change)

def update_date(root, date):
    """Find publicationStmt/date, update it."""
    try:
        publication = next(tei_iter(root, 'publicationStmt'))
    except StopIteration:
        print("File lacks a publicationStmt node, possibly malformed.")
        sys.exit(3)
    try:
        date_node = next(tei_iter(publication, 'date'))
    except StopIteration:
        return
    date_node.attrib['when'] = date
    date_node.text = datetime.datetime.strptime(date, '%Y-%m-%d').strftime('%b %d, %Y')

def update_extent(root):
    try:
        extent = next(tei_iter(root, 'extent'))
    except StopIteration:
        print("Could not find extent tag, possibly malformed file.")
        sys.exit(3)
    extent.text = '%i headwords' % sum(1 for _ in tei_iter(root, 'entry'))


def update_copyright(root):
    """Find a stanza containing "(c) 2014-2017 xyz" or "© 2020 foo" to update
    the year."""
    try:
        availability = next(tei_iter(root, 'availability'))
    except StopIteration:
        print("Unable to find availability tag, possibly malformed TEI file.")
        sys.exit(3)
    copyright = re.compile(r'(©|\([cC]\))\s*([0-9]{4})(?:-)([0-9]{4})')
    for tag in availability.iter():
        match = copyright.search(tag.text)
        if not match:
            continue
        start, end = match.span()
        year = match.groups()[1]
        if match.groups()[2]:
            year = match.groups()[2]
        tag.text = tag.text[:start] + tag.text[start:end].replace(year,
                    datetime.datetime.now().strftime('%Y')) + \
                tag.text[end:]
        return # only update first copyright stanza, hopefully most current one

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
    # ToDo: username, author, edition
    isodate = datetime.datetime.today().strftime('%Y-%m-%d')
    username = 'humenda'
    tree = XmlParserWrapper(input_file)
    try:
        revision_desc = next(tei_iter(tree.root, 'revisionDesc'))
    except StopIteration:
        print("Error, could not find revisionDesc.")
        sys.exit(20)
    add_changelog_entry(revision_desc, edition, isodate, username)
    update_date(tree.root, isodate)
    update_copyright(tree.root)
    update_extent(tree.root)
    tree.write(input_file)

main()
