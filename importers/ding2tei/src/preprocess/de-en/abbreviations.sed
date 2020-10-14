#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/abbreviations.sed - //-abbreviations
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


## "." -> /./

s`"(/\.ed)"$`/ \1 /`


## [abbr .] -> /./
# Found by searching for "Abk" and "abbr".

s`\[(Abk|abbr)\.: ([A-Za-z.-]+)\]`/\2/`g
s`\((Abk|abbr)\.: ([A-Za-z.-]+)\)`/\2/`g

s`, (Abk|abbr)\.: ([A-Za-z.]+)\)`) /\2/`g

# Do not generalize this.  There may be abbreviations containing slashes.
s`\[Abk\. M\./M\]`/M.; M/`g


## Misc

# <,> -> <.>
s`\<(signed) /(sgd),/`\1 /\2./`g

# Remove {}; guessing a little here.
s`/in \{prp\} \./`/in prp./`g

# Qustionably placed: 'A; B /a; b/' -> 'A /a/; B /b/'
s`^(der Ältere)\; (Senior) /(d\.Ä\.); (Sen\.)/`\1 /\3/\; \2 /\4/`

# [.] -> /[.]/
s`(^|:: *)(\[sic\])`\1/\2/`g

# This one is repeated, so possibly not a typo.
s`(supervised injection site) /SIS/Ms`\1s /SIS/s`
s`/SIS/M\>`/SIS/`g


# vi: noet
