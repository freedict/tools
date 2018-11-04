#!/usr/bin/env python3

import sys
import random
from xml.etree import ElementTree as etree

from ankisync.apkg import Apkg

NS = 'http://www.tei-c.org/ns/1.0'
NS_MAP = {'': NS, 't': NS}


def ns(name):
    return '{%s}%s' % (NS, name)


TAG_ENTRY = ns('entry')
TAG_HEADER = ns('teiHeader')


def parse_entry(element):
    orths = element.findall('./t:form//t:orth', NS_MAP)
    cits = element.findall('./t:sense//t:cit/t:quote', NS_MAP)
    return orths[0].text, ', '.join(c.text for c in cits)


assert len(sys.argv) == 2, 'Usage: tei2anki.py FROM-TO.tei'
input_filename = sys.argv[1]
output_filename = input_filename.replace('.tei', '.apkg')

entries = []
with open(input_filename) as input_file:
    for _, element in etree.iterparse(input_file):
        if element.tag == TAG_HEADER:
            deck_name = element.find('./t:fileDesc/t:titleStmt/t:title', NS_MAP).text
        if element.tag == TAG_ENTRY:
            entries.append(parse_entry(element))

with Apkg(output_filename) as apkg:
    model_id = apkg.init(
        first_model=dict(
            name='default model',
            fields=['source', 'target'],
            templates={
                'Forward': ('{{source}}', '{{source}} <hr id=answer> {{target}}'),
            }
        ),
        first_deck=deck_name
    )
    for source, target in random.sample(entries, 100):
        apkg.add_note({
            'modelName': 'default model',
            'deckName': deck_name,
            'fields': {
                'source': source,
                'target': target,
                }
            })
