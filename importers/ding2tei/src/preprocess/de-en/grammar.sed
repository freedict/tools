#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/grammar.sed - fix errors in grammar annotations
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


### Semantic errors

# Wrong pos annotation
s`\<(makrohumiphag) \{m\} (\[zool\.\])`\1 \{adj\} \2`
# - (de); translates to "macrohumiphagous" (en), which is an adjective
#   - source: https://doi.org/10.1016/0378-1127(95)03580-4



### Syntactic errors

## () -> {}
# Note: There are likely more of this kind.

s`\<(Felge|Glanzleinwand) \(f\)`\1 {f}`g
s`\<(Futterkattun) \(m\)`\1 {m}`g
s`\<(Normalnull) \(n\)`\1 {n}`g
s`\(adv\)`{adv}`g

# See also: alter.sed
s`\{pron\} \(relativ\)`{pron} {relativ}`g

s`\(\+ *(Gen|Akk|Dat)\.?\)`\{+\1.\}`g

s`\{prep\}`{prp}`g


## [] -> {} (and nomalization)

s`\[kein Plural\]`\{no pl\}`g

# Note: {no sing} never occurs, it is infered from {no pl} and {sing}.
s`\[only plural\]`{no sing}`g
s`\[no singular\]`{no sing}`g


## Superfluous <.>.

s`\{(adv)\.\}`{\1}`g


## (.) {.} -> (. {.})

s`\((sich)\) \{(Dat\.)\} (die Sonnenenergie nutzbar machen)\>`(\1 {\2}) \3`g


## {.} . -> . {.}

s`:: (\{adv\}) (idiosyncratically)$`:: \2 \1`
s`:: (\{adj\}) (conflicting)\;`:: \2 \1\;`


## <+> outside {.}

s`\{prp\} \+ (which/what)\; \1 \+ \{prp\}`{prp +} \1; \1 {+ prp}`g


## Consistent spacing and "dotting".

# Note: This could also be delegated to the parser.  Doing it here simplifies
#       matching keywords at the lexer stage, since '+' and '.' can be
#       considered part of the (unique) keyword.
s`\{\+ *(Gen|Akk|Dat)\.?\}`\{+\1.\}`g
s`\{\+ *(conj)\}`{+conj}`g


# vi: noet
