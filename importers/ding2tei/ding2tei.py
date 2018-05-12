#!/usr/bin/env python3
# (C) 2017 Sebastian Humenda
# License: GPL3+
"""
Ding2tei works in two phases:

1.  Split each dictionary definition (one per line) into headwords and
    translations, then tokenize the most important bits:

    -   {…} - grammatical info
    -   […] category
    -   (…) anything, really
    -   "|", "," and ";" cause extra events

Afterwards, the tokens are restructured into an dictstructure format,
which resembles roughly the TEI format, but allows for an more abstract
handling. This abstract representation, once constructed, can do
wild-guessing on element content to figure out details of the plain text
notation. I.e. whether (…) contain a collocate word or smash to
translation into one. Some of this wild-guessing is language-specific
and might need additional work for a specific dictionary. For instance,
"waste (disposal) consultant" → "waste consultants" and "waste disposal
consultants".
"""

import os
import re
import sys

import languages
import tei

from fd_import import tokenizer
import fd_import.output



# ToDo: fancy auto-discovery?
DICT_MODULES = {'deu-eng': languages.DeuEngParser,
        'spa-deu': languages.SpaDeuParser}


def main(input_path, tei_file, output_directory):
    fname = re.search("^([a-z]{3}-[a-z]{3})(?:\\..+)?", os.path.basename(input_path))
    if not fname:
        raise OSError(("The input file should be renamed to follow the FreeDict "
            "naming convention (xxx-yyy.extension, so that the correct parser "
            "module can be detected."))
    dict_name = fname.groups()[0]
    try:
        t = DICT_MODULES[dict_name]()
    except KeyError:
        print("Sorry, but there is no parser for %s." % fname.groups()[0])
        sys.exit(5)


    entries = []
    lnum = None
    with open(input_path, encoding='utf-8') as f:
        for lnum, line in enumerate(l.strip() for l in f if not l.startswith('#')):
            tokens = [tokenizer.tokenize(part, parse_slash=True) for part in line.split(' :: ')]
            node = tei.entry2xml(t.parse(tokens)) # both headwords + translations
            entries.append(node)
    root = tei.attach_xml_body(tei_file, entries)

    if not os.path.exists(output_directory):
        os.makedirs(output_directory)
    tei_fn = os.path.join(output_directory, dict_name + '.tei')
    with open(tei_fn, 'wb') as output:
        root.write(output, encoding="utf-8", xml_declaration=None)
        output.write(b'\n')
    print("Reindenting file")
    fd_import.output.reindent_xml(tei_fn)
    print(lnum, "entries written.")
    fd_import.output.copy_readme(input_path, output_directory)
    fd_import.output.mk_makefile(output_directory, [os.path.basename(tei_fn)])

if __name__ == '__main__':
    if len(sys.argv) != 4:
        print("Usage: %s <INPUT_FILE> <TEI_FILE> <OUTPUT_DIRECTORY>" % sys.argv[0])
        print("\nINPUT_FILE has to be in the ding format.")
        print(("TEI_FILE can be either a skeleton with an empty body tag or a "
                "full dictionary\n  (where the body will be ignored)."))
        print("  The head of this file will become the new header of the generated dictionary.")
        print(("OUTPUT_DIRECTORY is the directory where files will be written "
            "too. This script\n  will overwrite existing files."))
        sys.exit(1)
    for file in sys.argv[1:3]:
        if not os.path.exists(file):
            sys.stderr.write("Error: %s doesn't exist.\n" % file)
            sys.exit(2)

    main(sys.argv[1], sys.argv[2], sys.argv[3])

