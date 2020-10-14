#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/slashes.sed - fix wrong spacing around slahes
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
#  * See doc/syntax.slashes.
#  * TODO: Consider to generalize a few and in particular also catch wrongly
#          placed weak/strong slashes.

s`(ban on entering the house)//(pub/\.\.\.)`\1/\2`

s`\((bei etw\.)/ (gegenüber etw\.)\)`(\1 / \2)`g
s`\((in sth\.)/ (from sth\.)\)`(\1 / \2)`g
s`\((von etw\.)/ (zu etw\.)\)`(\1 / \2)`g
s`\((of sth\.)/(for sth\.)\)`(\1 / \2)`g

s`(Kein) / (Keine) (\[Ös\.\] Ausschank)`\1/\2 \3`g

s`\<(visual tree assessment) /(VTA) /`\1 /\2/`
s`\<(this order/custom\.)/ (You have)\>`\1 / \2`
s`/MFM$`&/`
s`\<(to understand) /(recognise/grasp the)`\1/\2`
s`\<(iron hand)/ (iron fist)\>`\1 / \2`g
s`\<(Kreuzschritt \{m\} vor)/ (zurück)\>`\1/\2`
s`\<(declaration) /(recognition) (of a claim)\>`\1/\2 \3`
s`\<(on) (sb\.)/ (sth\.)`\1 \2/\3`g
s`\<(centre \[Br\.\])/ (center \[Am\.\])`\1/\2`g
s`\<(center \[Am\.\]) /(centre \[Br\.\])`\1/\2`g
s`\<(easing) /(reduction) (in)\>`\1/\2 \3`g
s`\<(Geräte \{pl\}) /(Ausrüstung \{f\})`\1/\2`g

# TODO: generalize?
s`\<(to) (approach)/ (consider)\>`\1 \2/\3`g

s`\<(governor) (/Gov\./s)\>`\1s \2`g
s`\<(mit jdm\.)/ (mit etw\.)`\1 / \2`g
s`\<(with sb\.)/ (in sth\.)`\1 / \2`g
s`(\(Plectranthus coleoides/forsteri)/ (glabratus\))`\1/\2`
s`\<(timber \[Br\.\])/ (lumber \[Am\.\])`\1/\2`g
s`\<(zurückbekommen)/ (zurückerhalten)\>`\1/\2`g
s`\<(of sth\.)/ (in )`\1 / \2`g
s`\<(fibre \[Br\.\])/ (fiber \[Am\.\])`\1/\2`g
s`\<(skivvy \[Br\.\])/ (slave)`\1/\2`g
s`\<(armoured \[Br\.\])/ (armored \[Am\.\])`\1/\2`g
s`\<(to take)/ (to carry out) (a measure)\>`\1 / \2 \3`g
s`\<(action for possession \[Br\.\])/ (action of eviction \[Am\.\])`\1 / \2`g
s`\<(text-book example of how)/ (an object lesson in how)\>`\1 / \2`g
s`\<(photovoltaics) /(PV)\; (solar)\>`\1 /\2/\; \3`
s`\<(jdn\.)/ (etw\.)`\1/\2`g
s`\<(Mehrzweckeinsatzstock \{m\}) /(MES) /`\1 /\2/`g
s`\((of) /(showing sth\.)\)`(\1 / \2)`g
s`\<(and don't get into mischief) /(and keep out of mischief)\>`\1 / \2`g
s`\<(limitation) /(lapse) (of time)\>`\1/\2 \3`g
s`\<(Abschluss \{m\}) /(Abschließen \{n\})`\1/\2`g
s`\<(wind power unit) / (WPU)/`\1 /\2/`g
s`\<(in) (Bestzustand) /(1a-Zustand)\>`\1 \2/\3`g
s`\<(to hold the floor)/ (to address the meeting)\>`\1 / \2`g
s`\<(balsam)/ (myrobalan)\>`\1/\2`g
s`\<(Zeitungen) /(Zeitschriften)\>`\1/\2`g
s`\<(are at a critical juncture)/ (have reached a critical juncture)\>`\1 / \2`
s`\((of sb\.)/ (of), (from) (sth\.)\)`(\1 / \2/\3 \4)`g
s`\<(set up) /(discontinue)\>`\1 / \2`g
s`\<(als etw\.)/(zugunsten von jdm\.)`\1 / \2`g
s`\<(as sb\.)/ (in favour of sb\.)`\1 / \2`g
s`\<(gun) /(missile)\>`\1/\2`g
s`\<(into sth\.)/ (into doing sth\.)`\1 / \2`g
s`\<(to proffer) (sb\. sth\.) /(sth\. to sb\.)`\1 \2 / \3`g
s`\<(of) (sb\.)/ (sth\.)`\1 \2/\3`g
s`\<(für jdn\.)/ (als) jd\.`\1 / \2 jdn.`g
s`\<(interpreted)/ (understood)\>`\1/\2`g
s`\<(in favour) (of)/ (against) (sth\.)`\1 \2/\3 \4`g
s`\<(on/upon/against) (sb\.)/ (sth\.)`\1 \2/\3`g
s`\<(etwas zu bedeuten haben)/ (etwas bedeuten)\>`\1 / \2`g
s`\<(sth\.)/ (doing sth\.)`\1 / \2`g
s`\<(einen Ort entlang) /(durch einen Ort)\>`\1 / \2`g
s`\<(mit jdm\.)/ (mit etw\.)\>`\1 / \2`g
s`\<(with sb\.)/ (in sth\.)`\1 / \2`g
s`\<(about sth\.)/ (for doing sth\.)`\1 / \2`g
s`\<(shop \[Br\.\]) /(store \[Am\.\])`\1/\2`g
s`\<(von etw\.)/ (gegen etw\.)`\1 / \2`g
s`\<(in die) (Normalform)/(kanonische Form)/? (gebracht|bringend?)\>`\1 \2 / \3 \4`g
s`\<(zerwuzelt \[Ös\.\])/ (einen Schranz in den Bauch gelacht)\>`\1 / \2`g
s`\<(look after)/ (take care of)\>`\1 / \2`g
s`\<(zu) (bedienen)/ (handhaben) (sein)\>`\1 \2/\3 \4`g
s`\<(home economics)/ (family and consumer sciences)\>`\1 / \2`g
s`\<(ein Angebot ausschlagen)/ (sich einer Antwort enthalten)\>`\1 / \2`g
s`\<(to sth\.)/ (to do sth\.)`\1 / \2`g
s`\<(zur rechten Zeit)/ (zum richtigen Zeitpunkt)`\1 / \2`g
s`\<(renovieren)/ (erneuern)`\1/\2`g
s`\<(take on) /(lay off)\>`\1 / \2`g
s`(sb\.)/ (in)`\1 / \2`g

# Note that there is a meaning in the different spacing, which is dropped here.
s`\<(tangled up)/(entangled)/ (snarled up)/(ensnared)`\1 / \2 / \3 / \4`g

s`\<(zum Erfolg)/ (zu seinem Glück)\>`\1 / \2`g
s`\<(to enable) (sb\.)/ (sth\.)`\1 \2/\3`g
s`\<(to transmogrify) (sb\.)/(sth\.)/ (into sth\.)`\1 \2/\3 / \4`g
s`\<(wenn/wiewohl es schon lange her ist\.) /(wenn es auch schon lange her ist\.)`\1 / \2`g
s`\<(threw) /(set) (back)\>`\1/\2 \3`g
s`\<(attached)/ (screwed on)\.`\1 / \2.`g
s`\((Kfz:) /([A-Z]+)\)`(\1 /\2/)`g
s`\<(Rutherfordium)/ (Kurtschatovium)\>`\1/\2`g

# Note: This is a modification.
s`\<(the) (biscuit \[Br\.\])/ the (cake \[Am\.\])`\1 \2/\3`g

s`\<(auf) (jdn\.)/ (etw\.) (setzen)\>`\1 \2/\3 \4`g
s`\((by Pilcher)/ (work title)\)`(\1 / \2)`g

s`(/Hg\(CNO\)2/)(\(Sprengstoff\))- `\1 \2\; `

s`\<(Mikro\.\.\.)(/µ/)`\1 \2`

s`/I/O/`/ I/O /`g
s`/a/s/l\?/`/ a/s/l? /`g

s`(diameter for fixing the disc \[Br\.\])/ (disk \[Am\.\] to the brackets)`\1 / \2`

s`(to make a run on the shops \[Br\.\]) /(stores \[Am\.\] \[fig\.\])`\1 / \2`
s`(receivables) / (Rec\.)/`\1 /\2/`

# Note: The following is broken in other ways (TODO).
s`\((\+Gen)/ (über etw\.)\)`(\1 / \2)`g


# vi: noet
