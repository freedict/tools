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

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(
    sys.argv[0]))))
import tokenizer



# ToDo: fancy auto-discovery?
DICT_MODULES = {'deu-eng': languages.DeuEngParser,
        'spa-deu': languages.SpaDeuParser}


def main(input_path):
    fname = re.search("^([a-z]{3}-[a-z]{3})(?:\..+)?", os.path.basename(input_path))
    if not fname:
        raise OSError(("The input file should be renamed to follow the FreeDict "
            "naming convention (xxx-yyy.extension, so that the correct parser "
            "module can be detected."))
    try:
        t = DICT_MODULES[fname.groups()[0]]()
    except KeyError:
        print("Sorry, but there is no parser for %s." % fname.groups()[0])
        sys.exit(5)
    with open(input_path, encoding='utf-8') as f:
        lnum = None
        with open(os.path.splitext(input_path)[0] + '.tei', 'wb') as output:
            for lnum, line in enumerate(l.strip() for l in f if not l.startswith('#')):
                tokens = [tokenizer.tokenize(part, parse_slash=True) for part in line.split(' :: ')]
                entry = t.parse(tokens) # we pass [headwords, translations]
                xml = tei.entry2xml(entry)
                xml.write(output, encoding="utf-8", xml_declaration=None)
                output.write(b'\n')
        print(lnum, "entries written.")

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("Usage: %s <INPUT_FILE>" % sys.argv[0])
        print("\nThe input file must be in the ding format.")
        sys.exit(1)
    if not os.path.exists(sys.argv[1]):
        sys.stderr.write("Error: %s doesn't exist." % sys.argv[1])
        sys.exit(2)

    main(sys.argv[1])
