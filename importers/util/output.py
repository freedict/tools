"""This module contains functions to write all accompanying files for the
dictionary, e.g. license or README."""

import os
import re
import shutil
import xml.dom.minidom

class OutputError(Exception):
    pass

def copy_readme(input_file, output_directory):
    """When a README is found in the same directory as `input_file`, the file is
copied to OUTPUT_DIRECTORY. The input file has to be called "README.xxx-yyy" or
"README.xxx-yyy.EXT", where ext is anarbitrari file extension and xxx-yyy a ISO
6639-3 three-letter language code."""

    full_input = os.path.abspath(input_file)
    input_file = os.path.basename(input_file)
    input_dir = os.path.dirname(full_input)
    iso_code = re.search('^([a-z]{3}-[a-z]{3})', input_file)
    if not iso_code:
        raise OutputError(('%s does not follow the "xxx-yyy.EXT" '
            'naming convention.') % input_file)
    iso_code = iso_code.groups()[0]
    readme = None
    for match in (re.search('^README.([a-z]{3}-[a-z]{3}).*', f)
            for f in os.listdir(input_dir)):
        if not match:
            continue
        if match.groups()[0] == iso_code:
            readme = match.string
            break
    if readme:
        shutil.copy(os.path.join(input_dir, readme),
                os.path.join(output_directory, 'README.md'))
    else:
        raise OutputError("No README found.")


def mk_makefile(output_path, additional_files=None):
    readme = [f for f in os.listdir(output_path) if f.lower().startswith('readme')]
    if readme:
        if not additional_files:
            additional_files = []
        additional_files.append(readme[0])

    additional_files = ('' if not additional_files
        else ' '.join(additional_files))
    with open(os.path.join(output_path, 'Makefile'), 'w', encoding='utf-8') as f:
        f.write("""# The line below is really just a fallback and only works if you have got a copy
# of the tools directory at this location. It's better to set the environment
# variable in your shell.
FREEDICT_TOOLS ?= ../../tools
DISTFILES = COPYING %s \
        freedict-P5.xml freedict-P5.rng freedict-P5.dtd freedict-dictionary.css\
        INSTALL Makefile NEWS
# do not generate phonemes
supported_phonetics =

include $(FREEDICT_TOOLS)/mk/dicts.mk
""" % additional_files)

def reindent_xml(file_name):
    """Reindent an XML file with Python's minidom package. Please note that this
    will likely use a large amount of main memory."""
    tree = xml.dom.minidom.parse(file_name)
    with open(file_name, 'w', encoding='utf-8') as f:
        reindented = tree.toprettyxml(indent="  ")
        # avoid multiple empty lines, inserted by minidom
        f.write('\n'.join(l for l in reindented.split('\n') if l.strip()))


