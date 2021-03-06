This importer is for a single dictionary format only, as used by the epo-eng
dictionary from
<http://www.denisowski.org/Esperanto/ESPDIC/espdic_readme.htm>.
It is (c) 2015 Paul Denisowski, under the terms of the Creative Commons Attribution
3.0 Unported License.

Usage
-----

The usage of this script is extremely simple:

    $ python3 epo-eng-import.py <UPSTREAM_DICT> <TEI_SKELETON> <OUT_DIR>

-   `UPSTREAM_DICT`: path to the text dictionary, as distributed by the upstream
    project
-   `TEI_SKELETON`: header information; should be taken from the last release
    and extended with the changes with the newer version.
-   `OUT_DIR`: output directory, must not exist.

Python3 is the name of the Python interpreter. It might be that your systems
calls a Python version > 3.x just `python`, then you need to alter the command.

Sources
-------

The markup of the original Esperanto-English dictionary is hard to parse. The
current parser is far from perfect, but it does a fairly good job. I've
contacted upstream regarding a patch (Jul 2017), but haven't got a response.
Therefore, the changes are recorded in [epo-eng.patch](epo-eng.patch) and can be
applied to the epo-eng dictionary released in 2015.

The file README.dict is copied as README to the output directory.
