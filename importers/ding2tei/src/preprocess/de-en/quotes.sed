#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/quotes.sed - regularize quotes
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


## Quotes
# Notes:
# * There is no clear distinction between single <'> and double <"> quotes.
#   Sometimes they are even used together (e.g. <"abc'>).
#   * TODO: It might be necessary to identify them.
#   * When revising this file for version 1.9, I got the impression that <'>
#     is the standard on the English side, while <„> and <“> are standard on
#     the German side.
# * Sometimes (almost always?) '„“' is used on the german side
#   (e.g., „So für den Hausgebrauch“).
#   - Apparently new convention in version 1.9.
# * Try to infer the "correct" quote variant from the surrounding.
# * <"> may also be used as abbreviation for the unit inch.
# * <'> may also be used in other contexts (e.g., <he'll>, <f'>).
#   * Usage as apostroph is quite frequent.

s`"(Are you a good singer/player\?)'`'\1'`
s`'(Pay per click)"`'\1'`g
s`"(The Loyal Subject)'`'\1'`g

s`"(The Hunchback of Notre-Dame), (1831)`"\1", \2`g


# vi: noet
