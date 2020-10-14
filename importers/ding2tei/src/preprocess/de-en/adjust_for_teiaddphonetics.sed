#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/adjust_for_teiaddphonetics.sed - adapt for the bugs in
#                                                   teiaddphonetics
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

# teiaddphonetics (espeak-ng) fails with the below pattern unaltered.
#  * Actually, it seems fine if there is no [[:alphanum:]] somewhere left
#    of the below pattern, in the enclosing unit.  This is unlikely to ever
#    occur in practice though (and it does not in version 1.8.1 of the Ding).
#  * The alteration is not very nice, but likely the least invasive that makes
#    teiaddphonetics work.
#  * One might also consider to make this alteration on the TEI output.
#    * Note that the pattern is only a problem, when part of an element that is
#      to be amended with phonetics information (i.e., when part of a
#      headword).
#  * Unsure, whether the below actually catches all problematic cases
#    (teiaddphonetics usually runs for 20 hours or so before spitting out the
#    first error.)
#    * Run teiaddphonetics with `--espeak-count 1' to get a proper error
#      message.
s`([!?:,])(["'])`\1 \2`g


# vi: noet
