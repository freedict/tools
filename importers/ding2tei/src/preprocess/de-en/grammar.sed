#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/grammar.sed - fix errors in grammar annotations
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


### Semantic errors

# Typo in grammatical gender
s`(Aggregationsreagenz) \{d\}`\1 \{n\}`g

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


## [] -> {}

s`\[\+ *Genitiv\]`{+Gen.}`g

s`\[(vi|pl)\]`\{\1\}`g
s`\[kein Plural\]`\{no pl\}`g

# Note: {no sing} never occurs, it is infered from {no pl} and {sing}.
s`\[only plural\]`{no sing}`g
s`\[no singular\]`{no sing}`g

s`(entsprechend etw\.) \[Dativ\]`\1 {+Dat.}`g

s`\[im Genitiv\]`{Gen.}`g


## Missing {}

s`\<(die Summe) \+ Gen (\[math\.\])`\1 \{+Gen.\} \2`


## Normalization

# Occurs once only:
s`\{prep\.\}`{prp}`g


## Consistent spacing and "dotting".

# Note: This could also be delegated to the parser.  Doing it here simplifies
#       matching keywords at the lexer stage, since '+' and '.' can be
#       considered part of the (unique) keyword.
s`\{\+ *(Gen|Akk|Dat)\.?\}`\{+\1.\}`g
s`\{\+ *(conj)\}`{+conj}`g

s`\{(adj)\.\}`{\1}`g


## Semicolon between prp and {+<case>}.

s`\{(prp) *\+ *(Gen|Akk|Dat)\.\}`{\1\; +\2.}`g



## Incosistent use of separators between 'vi' and 'vt'.

# There is exactly one occurence of each of <,>, </>, <;>.
# I decide to use <,>, as between genders (same meaning of the separator: or).
# Note however the frequent use of "{prop; ...}" (meaning: and).
# Note that using <,> for all grammar annotations is not an option, since
# `+<case>' annotations may be further annotated with interogative pronouns,
# where <,> is used as a separator.
# One might instead consider to make <;> the default.  This is what the pretty-
# printer curretly does.  Alternatively, one would have to allow <,> (and </>)
# only for certain subsets of annotations (in practice: v.*, {m,n,f,pl}).

s`\{(vi)/(vt)\}`\{\1,\2\}`g
s`\{(vt)\; *(vi)\}`\{\1,\2\}`g


# vi: noet
