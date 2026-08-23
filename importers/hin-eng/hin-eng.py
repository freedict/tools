"""
This scipt converts the https://www.cfilt.iitb.ac.in/~hdict/webinterface_user/download.php?get=UW_Hindi_Dict_20131003.zip
 to TEI5 required format.
"""

import dataclasses
import argparse
import os
import zipfile
from collections import defaultdict
from pathlib import Path
import xml.etree.ElementTree as ET

import requests

HINDIDICT = True
URL = "https://www.cfilt.iitb.ac.in/~hdict/webinterface_user/download.php?get=UW_Hindi_Dict_20131003.zip"
ZIPFILE = "UW_Hindi_Dict_20131003.zip"
DICTFOLDER = "UW_Hindi_Dict_20131003"
TEI_FILE = "header.tei"
POS_TAG_MAPPING = {
    "CONJ": "conj",
    "N": "n",
    "ADJ": "adj",
    "V": "v",
    "ADV": "adv",
    "PREP": "prep",
    "PRON": "pron",
}
GENDER_MAPPING = {
    "M": "m",
    "F": "f",
}
OUTPUT_FILE = "hin-eng.tei"


class Stats:
    attributes: set[str] = set()
    semantics: set[str] = set()

    def add(self, entry: Entry) -> None:
        self.attributes.update(entry.attributes)
        self.semantics.update(entry.semantics)


@dataclasses.dataclass
class Translation:
    lang: str
    meaning: str


@dataclasses.dataclass
class Entry:
    head: str
    lang: str
    translation: Translation
    semantics: str
    attributes: list[str]
    sample: str
    pos_tag: str = dataclasses.field(default="", init=False)
    gender: str = dataclasses.field(default="", init=False)

    def __post_init__(self) -> None:
        self.parse_attributes()

    def __str__(self) -> str:
        return (
            f"Head: {self.head}\n"
            f"Translation: {self.translation}\n"
            f"Attributes: {self.attributes}\n"
            f"Semantics: {self.semantics}\n"
        )

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Entry):
            return NotImplemented
        return (self.pos_tag, self.head, self.gender) == (
            other.pos_tag,
            other.head,
            other.gender,
        )

    def __hash__(self) -> int:
        return hash((self.pos_tag + "_" + self.head + "_" + self.gender))

    def get_id(self) -> str:
        res = ""
        if self.pos_tag:
            res += self.pos_tag + "_"
        res += self.head
        if self.gender:
            res += self.gender + "_"
        return res.replace(" ", "_")

    def parse_attributes(self) -> None:
        if self.attributes[0].upper() in POS_TAG_MAPPING:
            self.pos_tag = POS_TAG_MAPPING[self.attributes[0].upper()]
        if len(self.attributes) <= 1 or self.pos_tag == "v":
            return
        for i, attr in enumerate(self.attributes[1:]):
            if attr in GENDER_MAPPING:
                self.gender = GENDER_MAPPING[attr]
                break


def download_zipfile() -> None:
    headers = {
        "User-Agent": "curl/8.0.1",
    }

    r = requests.get(
        URL,
        headers=headers,
        timeout=30,
        allow_redirects=False,
    )

    with open(ZIPFILE, "wb") as f:
        f.write(r.content)
    unzip()


def unzip() -> None:
    zip_ref = zipfile.ZipFile(ZIPFILE, "r")
    zip_ref.extractall(DICTFOLDER)
    zip_ref.close()


def save_tei_file() -> None:
    pass


def create_xml(entries: defaultdict[Entry, list[Entry]]) -> None:
    print("Creating xml/tei file...")
    ns = {"tei": "http://www.tei-c.org/ns/1.0"}
    tree = ET.parse(TEI_FILE)
    root = tree.getroot()
    body = root.find(".//tei:body", ns)
    if body is None:
        return
    # todo take care of cases, (is verb gender different?), Plural sing, 1-3 person
    # key entry is also in other!
    for i, (key_entry, other) in enumerate(entries.items()):
        xml_id = key_entry.get_id()
        entry_elem = ET.SubElement(
            body, "entry", attrib={"xml:id": xml_id, "xml:lang": "en"}
        )
        form_elem = ET.SubElement(entry_elem, "form", attrib={"xml:lang": "hin"})
        orth_elem = ET.SubElement(form_elem, "orth")
        orth_elem.text = key_entry.head
        # create gramgroup
        gramGrp_elem = ET.SubElement(
            entry_elem,
            "gramGrp",
        )
        pos_elem = ET.SubElement(gramGrp_elem, "pos")
        pos_elem.text = key_entry.pos_tag  # todo handle <number> <per> etc
        if key_entry.gender:
            gender_elem = ET.SubElement(gramGrp_elem, "gender")
            gender_elem.text = key_entry.gender
        # todo handle multiple senses for translation in translations
        sense_elem = ET.SubElement(entry_elem, "sense")
        for entry in other:
            cit_type_elem = ET.SubElement(sense_elem, "cit", attrib={"type": "trans"})
            quote_elem = ET.SubElement(cit_type_elem, "quote")
            quote_elem.text = entry.translation.meaning
            if entry.semantics:
                usage_elem = ET.SubElement(sense_elem, "usg", attrib={"type": "hint"})
                usage_elem.text = entry.semantics  # TODO maybe change?
            if entry.sample:
                cit_example_elem = ET.SubElement(
                    sense_elem, "cit", attrib={"type": "example"}
                )
                sample_elem = ET.SubElement(cit_example_elem, "quote")
                sample_elem.text = entry.sample
    ET.indent(tree, space="  ", level=0)
    tree.write(
        OUTPUT_FILE,
        encoding="utf-8",
        xml_declaration=True,
    )
    print(f"Wrote {OUTPUT_FILE}")


def _get_translation(line: str) -> tuple[str, int]:
    translation = line.split('"', maxsplit=2)[1]
    if (
        "<" in translation
        or ">" in translation
        or "(" in translation
        or ")" in translation
    ):
        # semantic rules present
        translation = translation.split("(", maxsplit=1)[0]
    translation = translation.strip('"')
    if translation == "":
        return translation, 0
    return translation, line.find(translation) + len(translation)


def _get_semantic_restriction(line: str, index: int) -> tuple[str, int]:
    if "(" in line[index:]:
        sr = line[index:].split("(", maxsplit=1)[1]
    else:
        # rare format:  [HINDI]{}"ENGLISH"TAGS)<H,0,0>;
        sr = line[index:].split(")", maxsplit=1)[0].strip('"')
    sr = sr.split(")", maxsplit=1)[0]
    i = line.find(sr) + len(sr)

    if ">" not in sr and "<" not in sr and "icl>" not in sr:
        # actually a POSTAG!!!
        sr = ""
        i = 0

    return sr.strip("("), i


def _get_attributes(line: str, index: int) -> tuple[str, int]:
    if "(" in line[index:]:
        attributes = line[index:].split("(", maxsplit=1)[1]
    else:
        _tmp = line[index:].split("H,0,0>;")[0].strip("<")
        if '"' in _tmp:
            # special weird case '{}"ENGLISH"ATTRIBUTES)<H,0,0>;
            attributes = _tmp.split('"', maxsplit=1)[1].strip(")")
            return attributes, line.find(attributes) + len(attributes)
        else:  # there were only semantics no attribute
            return "", 0
    attributes = attributes.split(")", maxsplit=1)[0]
    i = line.find(attributes) + len(attributes)
    if attributes == "":
        i = 0
    return attributes, i


def parse_line(line: str, stats: Stats) -> Entry | None:
    """
    Parse each line into an entry by splitting.
    """
    head, line = line.split("]", maxsplit=1)
    head = head.strip("[")
    if head == "":
        return None
    translation, index = _get_translation(line)
    semantics, index = _get_semantic_restriction(line, index)
    attributes, index = _get_attributes(line, index)
    sample = line.rsplit(">;", maxsplit=1)[1]
    entry = Entry(
        head=head,
        lang="hin",
        translation=Translation(lang="en", meaning=translation),
        semantics=semantics,
        attributes=attributes.replace(".", ",").split(","),
        sample=sample,
    )
    stats.add(entry)  # TODO remove
    return entry


def iterate_files() -> list[Entry]:
    entries = []
    stats = Stats()
    print("Reading files...")
    for file in os.listdir(DICTFOLDER):
        with open(DICTFOLDER + "/" + file, "r") as f:
            if not file.endswith(".txt"):
                continue
            old_line = ""
            for line in f:
                line = (
                    line.strip()
                    .replace("”", '"')
                    .replace("\ufeff", "")
                    .replace("\u200b", "")
                )
                if "H,0,0>;" not in line:
                    old_line += line
                    continue
                if old_line:
                    old_line += line
                    line = old_line
                entry = parse_line(line, stats)
                if entry is None:
                    continue
                entries.append(entry)
                old_line = ""

    return entries


def convert2dict(entries: list[Entry]) -> defaultdict[Entry, list[Entry]]:
    from collections import defaultdict

    dictionary: defaultdict[Entry, list[Entry]] = defaultdict(list)
    for entry in entries:
        dictionary[entry].append(entry)
    return dictionary


def run() -> None:
    if not os.path.exists(DICTFOLDER) or len(os.listdir(DICTFOLDER)) == 0:
        download_zipfile()
    entries = iterate_files()
    dictionary = convert2dict(entries)
    create_xml(dictionary)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--path", type=Path, default="UW_Hindi_Dict_20131003")
    args = parser.parse_args()
    run()
