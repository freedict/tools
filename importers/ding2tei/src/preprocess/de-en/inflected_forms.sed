#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/inflected_forms.sed
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


### Annotated inflected forms (on the English side)


## </> -> <,>  (normalize inner separator)
s`\{(chided)/(chid)\; (chided)/(chidden)/(chid\})`\{\1, \2\; \3, \4, \5\}`g
s`\{(got)\; (got)/(gotten \[Am\.\])\}`\{\1\; \2, \3\}`g
s`\{(outshined)/(outshone)\; (outshined)/(outshone)\}`\{\1, \2\; \3, \4\}`g


## Alternative form in parantheses instead of separated by comma
s`\{(swung) \((swang \[obs\.\])\)\; (swung)\}`\{\1, \2\; \3\}`g

# Do not allow multiple conjugation annotations.
# In this particular case it is also unclear (from the syntax only), which
# forms the [archaic]-annotation applies to.
s`\{(work)\; (worked)\} \{(wrought)\; (wrought) (\[archaic\])\}`\{\1, \3 \5; \2, \4 \5\}`


## <;> -> <,>  (remove syntax breaking second <;>)
s`\{(besought, beseeched)\; (besought)\; (beseeched)\}`{\1\; \2, \3}`g
s`\{(awoke, awaked)\; (awoken)\; (awaked)\}`{\1\; \2, \3}`g
s`\{(pleaded)\; (pled \[coll\.\])\; (pleaded)\; (pled \[coll\.\])\}`{\1, \2; \3, \4}`g
s`\{(undergirded, undergirt)\; (undergirded)\; (undergirt)\}`{\1\; \2, \3}`g

# See: https://en.wiktionary.org/wiki/durst#English // 2020-09-09 00:34:20 CEST
s`\{(dared)\; (durst( \[obs\.\])?)\; (dared)\}`{\1, \2\; \4}`g


## <,> -> <;>
s`\{(hung), (hung)\}`{\1; \2}`g
s`\{(strung), (strung)\}`{\1; \2}`g
s`\{(kenned), (kent)\}`{\1; \2}`g


## Double }}.
s`(chid)\}\}`\1\}`


# vi: noet
