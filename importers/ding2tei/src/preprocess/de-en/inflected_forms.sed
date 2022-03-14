#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/inflected_forms.sed
#
# Copyright 2020,2022 Einhard Leichtfuß
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


### Annotated inflected forms (on the English side)


## Incorrect syntax and semantics.

# See <https://en.wiktionary.org/w/index.php?title=strive&oldid=65625808#Verb>.
s`\{(strove), (striven) / (strived)\; (strived)\}`\{\1, \3\; \2, \4, \1 [coll.]\}`

# See <https://en.wiktionary.org/w/index.php?title=stink&oldid=65417348#Verb>.
s`\{(stank)/(stunk), (stunk)\}`{\1, \2; \3}`g


## </> -> <,>  (normalize inner separator)

s`\{(chided)/(chid)\; (chided)/(chidden)/(chid\})`\{\1, \2\; \3, \4, \5\}`g
s`\{(got)\; (got)/(gotten \[Am\.\])\}`\{\1\; \2, \3\}`g
s`\{(outshined)/(outshone)\; (outshined)/(outshone)\}`\{\1, \2\; \3, \4\}`g
s`\{(leaped)/(leapt)\; (leaped)/(leapt)\}`{\1, \2\; \3, \4}`g
s`\{(smelled) / (smelt \[Br\.\])\; (smelled) / (smelt \[Br\.\])\}`{\1, \2\; \3, \4}`g
s`\{(learned) / (learnt \[Br\.\])\; (learned) / (learnt \[Br\.\])\}`{\1, \2\; \3, \4}`g
s`\{(staved)/(stove)\; (staved)/(stove)\}`{\1, \2\; \3, \4}`g
s`\{(swung) / (swang \[obs\.\]); (swung)\}`{\1, \2\; \3}`g
s`\{(struck)\; (struck) / (stricken \[Am\.\])\}`{\1\; \2, \3}`g
s`\{(girded)/(girt); (girded)/(girt)\}`{\1, \2\; \3, \4}`g


## Alternative form in parantheses instead of separated by comma
s`\{(swung) \((swang \[obs\.\])\)\; (swung)\}`\{\1, \2\; \3\}`g

# Do not allow multiple conjugation annotations.
# In this particular case it is also unclear (from the syntax only), which
# forms the [archaic]-annotation applies to.
s`\{(work)\; (worked)\} \{(wrought)\; (wrought) (\[archaic\])\}`\{\1, \3 \5; \2, \4 \5\}`


## Double }}.
s`(chid)\}\}`\1\}`


# vi: noet
