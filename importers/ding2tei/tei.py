"""This module contains the TEI conversion functionality. It can produce TEI XML
using the data types defined in the ditstructure module."""

from xml.etree import ElementTree as ET

class XmlStructureError(Exception):
    pass

def mknode(parent, tag, text=None, attrib=None):
    node = None
    if parent is None:
        node = ET.Element(tag)
    else:
        node = ET.SubElement(parent, tag)
    if text:
        node.text = text
    if attrib:
        node.attrib.update(attrib)
    return node

def attach_translation(sense, trans):
    if not sense.tag == 'sense':
        raise XmlStructureError("Translations can only be children of sense, " + \
                "got " + sense.tag)
    cit = mknode(sense, 'cit', attrib={'type': 'trans'})
    for text in trans.get_text():
        mknode(cit, 'quote', text=text)
    return cit

def attach_gramgrp(parent, gramgrp):
    gram = mknode(parent, 'gramGrp')
    if gramgrp.pos:
        mknode(gram, 'pos', text=gramgrp.pos)
    if gramgrp.gender:
        mknode(gram, 'gen', text=gramgrp.gender)
    if gramgrp.number:
        mknode(gram, 'number', text=gramgrp.number)
    if gramgrp.usg:
        mknode(gram, 'usg', text=gramgrp.usg)
    return gram

def attach_form(parent, form):
    node = mknode(parent, 'form', attrib=form.get_attributes())
    for orth in form.get_text():
        mknode(node, 'orth', text=orth)
    return node

def attach_sense(parent, sense):
    node = mknode(parent, 'sense')
    if sense.get_text():
        raise XmlStructureError("Sense with text detected, should be unused.")
    return node

def attach_usage(parent, usg):
    if len(usg.get_text()) > 1:
        raise XmlStructureError("Usg nodes cannot have more than one text entry.")
    return mknode(parent, 'usg', text=usg.get_text()[0])

def recurse_nodes(parent, structure):
    attach = lambda cls, parent, what: globals()['attach_' + \
            cls.__name__.split('.')[-1].lower()](parent, what)
    node = attach(structure.__class__, parent, structure)
    for child in structure.get_children():
        recurse_nodes(node, child)

def entry2xml(entry):
    entry_node = mknode(None, 'entry')
    for child in entry:
        recurse_nodes(entry_node, child)
    return entry_node

def attach_xml_body(tei_file, xml_entries):
    """Read given TEI XML file until the body tag. From there, insert the given
    entries. The result is a full TEI XML structure."""
    events = ET.iterparse(tei_file, events=["start"])
    root = next(events)[1]
    for _, elem in events:
        if elem.tag == 'body':
            break

    text = next(n for n in root if n.tag.endswith('text'))
    text.clear() # throw away all potential content
    body = ET.SubElement(text, 'body')
    for entry in xml_entries:
        body.append(entry)
    ET.register_namespace('', 'http://www.tei-c.org/ns/1.0')
    return ET.ElementTree(root)



