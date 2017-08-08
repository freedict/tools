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

sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(
    sys.argv[0]))))
import tokenizer

import languages
import tei


DICT_MODULES = {'deu-eng': languages.EngDeuParser}


def main(input_path):
    fname = re.search("^([a-z]{3}-[a-z]{3})(?:\..+)?", os.path.basename(input_path))
    if not fname:
        raise OSError(("The input file should be renamed to follow the FreeDict "
            "naming convention (xxx-yyy.extension, so that the correct parser "
            "module can be detected."))
    t = DICT_MODULES[fname.groups()[0]]()
    with open(input_path, encoding='utf-8') as f:
        for lnum, line in enumerate(l.strip() for l in f if not l.startswith('#')):
            tokens = [tokenizer.tokenize(part, parse_slash=True) for part in line.split(' :: ')]
            entry = t.parse(tokens) # we pass [headwords, translations]
            tree = tei.entry2xml(entry)
            import io
            with io.StringIO() as s:
                tree.write(s, encoding="unicode")
                s.seek(0)
                import xml.dom.minidom
                ret = xml.dom.minidom.parseString(s.read())
                print(ret.toprettyxml())
            # ToDo, use that stuff
            if lnum > 99:
                break

if __name__ == '__main__':
    main('deu-eng.txt')
