#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/typos.sed - fix some typos in the en-de Ding source
#
# Copyright 2020 Einhard Leichtfuß
#
# This file is part of ding2tei-haskell.
#
# ding2tei-haskell is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# ding2tei-haskell is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with ding2tei-haskell.  If not, see <https://www.gnu.org/licenses/>.
#

### Characters

# Quote characters from the Windows-1252 charset (convert to the Unicode
# version).  See <https://en.wikipedia.org/wiki/Windows-1252#Character_set>.
# The unicode values are 0x91 and 0x92 while the UTF-8 encodings are 0xC291 and
# 0xC292, respectively.
s`\xC2\x91`‘`g
s`\xC2\x92`’`g

# HTML code
s`\&#324\;`ń`g


### Simple typos

## Initial character to upper case

# Search regexes:
#  * '\b[a-zäöüß]+ \{(f|m|n|pl|no pl)\>.*::' (many false positives)

# Note that '\<' and '\>' do not generally suffice due to e.g. "...händler".
s` (ausfälle \{pl\}) ` \u\1`g
s` (instandhaltungseinschränkungen \{pl\})` \u\1`g
s` (tönung \{f\})` \u\1`g
s` (kleine) (auseinandersetzung \{f\})` \1 \u\2`g
s` (standringe \{pl\}) ` \u\1`g
s` (anreiz \{m\})` \u\1`g
s` (gleichrichten \{n\})` \u\1`g
s` (checksummen \{pl\})` \u\1`g


## Misc

s`\<Parsely\>`Parsley`g

s`(^| )(o)(ertermittlungsspezialist(en)?) `\1W\3`g

s` au(fahrtsstraßen) (\{pl\})` Auf\1 \2`

s` e(ngepasstheit) ` A\1 `g
s` erhöhunh ` Erhöhung `g

s`\<(Kameraassistent) (in \{f\}|innen \{pl\})`\1\2`g

s`\<(an etw), (riechen)\>`\1. \2`

s`\<i(Buche)\>`\1`g

s`/(Jun\.\; jun\.)\; (Jnr)\; (Jr),/`/\1\; \2.\; \3./`g

# Only on English side a misspelling (though on the german side it likely
# should be capitalized).
s`( :: .*)\<ressources\>`\1resources`g

s`\(teiweise\) (Änderung)\>`(teilweise) \1`g

# Dative, not accusative.
s`\<(zwischen) jdn\./etw\. und jdn\./etw\.`\1 jdm./etw. und jdm./etw.`g


# vi: noet
