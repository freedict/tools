This importer is for a single dictionary format only, as used by the epo-eng
dictionary from
<http://www.denisowski.org/Esperanto/ESPDIC/espdic_readme.htm>.
It is (c) 2015 Paul Denisowski, under the terms of the Creative Commons Attribution
3.0 Unported License.

Usage
-----

Save a fresh copy of the 6 October 2024 ESPDIC release as `espdic.txt` in
`importers/epo-eng/`. From that directory, apply the FreeDict patch once:

    patch --binary --forward --fuzz=0 espdic.txt -i epo-eng.patch

For this and subsequent imports, use the patched file directly:

    python3 epo-eng-import.py espdic.txt <TEI_SKELETON> <OUT_DIR>

-   `TEI_SKELETON`: the writable TEI skeleton from the last release, with an
    empty body. After a successful import the script updates it in place.
    The edition is the author’s release date (YYYY.MM.DD), the publication
    date is the local import day, and the extent counts imported entries.
    Each previously unrecorded release gets one revision credited to the
    local user.
-   `OUT_DIR`: output directory, must not exist.

Python3 is the name of the Python interpreter. It might be that your systems
calls a Python version > 3.x just `python`, then you need to alter the command.

Sources
-------

The markup of the original Esperanto-English dictionary is hard to parse. The
current parser is far from perfect, but it does a fairly good job. I've
contacted upstream regarding a patch (Jul 2017), but haven't got a response.
The corrections for the 6 October 2024 release are recorded in
[epo-eng.patch](epo-eng.patch).

The file README.dict is copied as README to the output directory.
