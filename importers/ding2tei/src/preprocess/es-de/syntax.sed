#!/usr/bin/env -S sed -Ef
#
# es-de/syntax.sed - modify the es-de dictionary to fit the Ding syntax
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

# Notes:
#  * This script is not a true solution.
#    * This concerns the header, "/..." suffixes and the "|" -> ";"
#      transformation.
#  * Instead, the parser should be adapted to separately allow for the
#    slightly different syntax of the es-de dictionary.
#  * The main program has some hard coded values pertaining to the German
#    English Ding dictionary, therefore even with this modifications, the
#    resulting TEI will have pretty wrong metadata.


# Grammar.
s`\{prn\}`\{pron\}`g
s` \{Demonstrativpronomen\}``
s`:: ::`::`
s`\{s\}`\{sing\}`g
s`\{prep\}`\{prp\}`g
s`\{([fnm])$`\{\1\}`
s`\;$``
s`\{mf\}`\{m,f\}`g
s`\{adv\.\}`{adv}`g


# Remove a superfluous <(>.
s`\<(Besetzung) \((\{f\}) (\(Film\))`\1 \2 \3`

s`^(retroceder \{v\} :: zurückziehen)\|$`\1`

/^monstruoso \{adj\} :: \{adj\}$/ d


# Adapt header
\`^# Spanish :: German word list$` d
s`^(# Version :: 0\.0i) Mon Mar  5 18:36:47 2012$`\1 2012-05-03`
s`^(# Copyright \(c\) :: .*) (2003-2012)$`\1,\n# \2`
\`^# License ::` {
	N; N
	s/\n# License :: ([^\n]+)/; \1/g
}
\`^# [0-9]+ entries$` d



# Debatable.
s` /([[:alpha:]]*)$` (\1)`


# Cheating...
s`\|`\;`g


# Remaining problems:
#  * <|> is always (and seemingly only) used to distinguish
#    different genders.  Sometimes, the respective other side has only one word
#    for both genders, i.e. the number of groups on both sides to not match.
#    . Ex.: "llorón {m}|llorona {f} :: Heulsuse {f} [coloq.]"
#    . Ex.: "logopeda {m,f} :: Logopäde {m} | Logopädin {f}"
#  * The header.

# vi: noet
