#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/usage.sed - fix errors in usage annotations
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


### Typos

s`\[texti\.]`[textil.]`g
s`\[bichem\.\]`[biochem.]`g
s`\[colcoll\.\]`[coll.]`g

s`\[Nordestdt\.\]`[Nordosttdt.]`g
s`\[(Nordt\.|Norddtd\.|norddt\.|Nordddt\.)\]`[Norddt.]`g
s`\[Süddtd\.\]`[Süddt.]`g



### () -> []

# TODO: Consider to catch this in the parser.
s`\((rarely used|rare)\)`[rare]`g


### Consistent spaces and dotting.
# Note: There's probably more of these (TODO).
#  - alternatively, let the parser take care of it.

# Missing <.>.
s`\[(Ös|Br|Am|Norddt|Mittelwestdt|Mitteldt|Dt|Bayr)\]`[\1.]`g
s`\[(ugs|cook|naut|photo|econ|coll|humor|adm|fig|stud|fin|ornith|meteo|geh|zool|textil|techn|pol|envir|bot|telco|statist|soc|sci|poet|mil|med|ling|hist|aviat|chem|zool|adm|biochem)\]`[\1.]`g

# Superfluous < > before <.>.
s`\[(soc|Am) \.\]`[\1.]`g

# Superfluous <.> (if necessary, decided by count that the dotless variant is
# correct).
s`\[(sport|print|auto|slang|dated)\.\]`[\1]`g



### Normalization
# Differently abbreviated annotations.
# Usually, the predominant version (by count) of two is taken.

## All except languages

s`\[rel.\]`\[relig.]`g
s`\[texil\]`[textil]`g
s`\[environ\.\]`[envir.]`g
s`\[TM\]`[tm]`g
s`\[(technical|tech\.)\]`[techn.]`g
s`\[stone\]`[min.]`g
s`\[Statistik\]`[statist\.]`g
s`\[sl\.\]`[slang]`g
s`\[milit\.\]`[mil.]`g
s`\[hum\.\]`[humor.]`g
s`\[gramm\.\]`[ling.]`g
s`\[finan\.\]`[fin.]`g
s`\[bio\.\]`[biol.]`g
s`\[const\.\]`[constr.]`g

# Not geogr. !
s`\<(Isogeotherme \{f\} ([A-Za-z ]+)) \[geo\.\]`\1 [geol.]`g

# This looks like [lit.], but it is not (infered from context).
# One occurence.
# TODO: Consider to make literary its own annotation (i.e, do not "fix")?
s`\[literary\]`[poet.]`g

# [astr.] Could also mean [astrol.] but does not for all occurences.
s`\[(astr\.|aston\.|atron\.)\]`[astron.]`g

# Note that the 'or' is converted in alter.sed.
s`\[(geh\.|formal) or humorous\]`[\1 or humor.]`g
s`\[(formal)/humorous\]`[\1/humor.]`g


## Languages

s`\[Scot.\]`[Sc.]`g
s`\[(NZ)\.\]`[\1]`g
s`\[New Zealand\]`[NZ]`g
s`\[Liecht\.\]`\[Lie.]`g
s`\[Sächsisch\]`[Sächs.]`g
s`\[(irisch|Irl\.)\]`[Ir\.]`g

# Debatable.  The replacement value occurs more frequently.
s`\[ZA\]`[South Africa]`g

# Debatable (Is there a semantic difference?).
# The replaced values occurs once, the replacing value 11 times.
s`\[Northern England\]`[Northern English]`g

# The replacing value does not occur.  It just should be an englisch
# expression / abbreviation, in my opionion.  Analogous to [Northern English].
s`\[Nordirl\.\]`[Northern Irish]`g

# All of the below versions of [Lat.] appear exactly once.  Use [Lat.] as it
# seems to match the general convention for dialects / languages the best.
s`\[(lateinisch|lat\.|Latin)\]`[Lat.]`g

# Replacing value does not occur.  Adapt to common naming scheme.
s`\[arabisch\]`[Arab.]`g


### Misc

# Doubled [art], one of them with a superfluous <.>.
s`\[art\.\] (\[art\])`\1`g


# vi: noet
