#!/usr/bin/env -S sed -Ef
#
# preprocess/de-en/misc.sed - fix some irregularities in the Ding source
#
# Copyright 2020-2022 Einhard Leichtfuß
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


## Abbreviations missing terminal <.>

# Exclude "so" ~ "so.", since it may very well not be an abbreviation.
s`\<(etw|jm?d[mns]?|sth|sb)\>($|[^.])`\1.\2`g


## Misplaced {}-annotations

s`(\(Kfz:) \{n\} (/AND/\))`\1 \2`


## <...>

s`< (\[mus\.\]) >`\1`g

# The only "<>"-annotation syntactically clearly on a unit level.
# Since semantically wrong, fix as follows.
# See https://en.wiktionary.org/wiki/delt // 2020-09-06 20:39:00 CEST
s`\<(to deal) \{(dealt)\; (dealt)\} <(delt)>`\1 {\2; \3, \4 [archaic]}`g

# There is one single occurence of an `s' after "<>".  I see no reason for it.
# Also, the first english form should be in plural, too.
s`\<(Atomforscher \{pl\}\; Atomforscherinnen \{pl\}) :: (.*) \| (atomic scientist) <(nuclear scientist)>s$`\1 :: \2 \| \3s <\4s>`

# Superfluous space.
s`\<(Türeinfassung \{f\} \[constr\.\]) <(Futterrahmen) >`\1 <\2>`


## Syntax-breaking typos (excl. slashes)

# Missing "<"
s` (Bucuresti)>` <\1>`g

# "->" -> "~"
s`\((superlative of) -> (level)\)`(\1 ~\2)`g


## [.] -> /

# Note: Information on predominance is lost.
# TODO: reconsider
s`\<(table) \[(tabular)\] (spar)`\1/\2 \3`g
s`\<(progressive) \[(prograde)\]`\1/\2`g
s`\<(spheroidal) (jointing) \[(parting)\]`\1 \2/\3`g
s`\<(axial) \[(optic)\] (angle)`\1/\2 \3`g
s`\<(tight) \[(close)\] (sand)`\1/\2 \3`g


# TODO: This is now really ugly.
s`\<(post) \[(nach|after)\]`\1/\2`g
s`\<(scrivere) \[(schreiben|writing)\]`\1/\2`g


## [.] -> (.)

# Optional words.  Information is lost.
# TODO: Consider to encode differently, e.g. [[]].

s`\<(eine Schar) \[(Personen)\]`\1 (\2)`g
s`\<(a clutch of) \[(persons)\]`\1 (\2)`g

# Unsure; possibly better translated to a slashed alternative.
s`\<(Brazilian) \[(optical)\] (pebble)\>`\1 (\2) \3`g

s`\<(to guzzle sth\.) \[(drink)\]`\1 (\2)`g
s`\<(to stitch) \[(book)\]`\1 (\2)`g

# Description.  Should become a <note>.
s`\<(Sauerkohl \{m\}) \[(in einigen Regionen alternativer Begriff zu Sauerkraut)\]`\1 (\2)`g
s`\<(listicle) \[(list \+ article)\]`\1 (\2)`g
s`\<(eine Portion) \[(Mengenangabe)\]`\1 (\2)`g


## Empty ()
s`\<(pathologische Abteilung \[med\.\]) \(\)`\1`


## Superfluous <;>
# - Note: Fixed in version 1.9; keep nonetheless.
s`\; *$``


## Smileys
# Note: There is exactly one smiley.  Make it fit the expected syntax.
#       (TODO/CONSIDER: alter expected syntax.)

s`/(:-\))/`/ \1 /`g

# This is not a smiley.  Possibly an alteration of syntax though.
s`\(@\)`/ @ /`g


## Mixed

# Superfluous semicolon; enrichment.
s`\<(note)\;\; (/N\.B\.\; NB/)`\1; nota bene [archaic] \2`


## Missing <;>

s`(Beide Schriftstücke sind online verfügbar\.) (Diese Schriftstücke sind beide online verfügbar\.)`\1; \2`


## Misc syntax errors

s`:: <> \|`:: |`g


## Further seemingly incorrect entries (TODO):
# - Note: most from version 1.8.1.

#ein gutes Blatt haben {n}							-- {} applies to part
#Freier, der den Autostrich abfährt {m} -- {} applies to whole / first part
#solange sie die Stelle innehat {n}     -- {} applies to ?
#Bouquet garni {n} cook									-- [cook.] ?
#Bemühen, das Gleiche zu erreichen {m}  -- {} applies to ?
#einen Unterhaltspfleger bestellen {m}  -- {} applies to part
# - alternative: disallow {} when refering to part of sentence (remove)
#eine Sache, die Probleme bereitet / Rätsel aufgibt {n}   -- ?
# - article ("eine") uncommon
# - {n} refers to "Rätsel" ?
#den Strom abschalten/ausschalten {pl}  -- ?
#äbtlich {m}														-- ?
#auktorial {n}
#eine Auskunft / Auskünfte einholen {n}
#bizarr {m}
#deckungsgleich sein {f}
#diese {pl}
# - first pl annotation on a non noun
#englandfreundlich {f}
#derjenige, der die Sache herausgibt {m} -- whole
#entomogamen {pl}
#etw. jodoformieren {n}
#leistungsgerecht {m}
#gut/schlecht schlafen {f}
# - use the </> somehow ?
# - {f} misplaced
#neapolitanisch {n}
#spiegelbildgleich {m}
# ... (TODO: search from here)
#obszön; verrucht, schlüpfrig; aufreizend {adj} :: raunchy
# - comma -> semicolon ?
#to write <> off sb./sth.
#auf das Angebot/die Zusage/den Vorschlag von jdm. eingehen
#etw./jdn. einführen; einweisen {vt} (in etw.)
# - does the prefix apply to both?
#guaranteed free from / of sth
# - should be a strong '/'
#jdn deputieren, etw. zu tun
# - parentheses ?
# - generalize in parser ?
#(+ Gen/von etw.)
#(in Fragen + Gen)
#as at + date
#lest + subjunctive
#(+ {adj})
#(+ comparative adjective)
#as a matter of fact + do; actually + do
#(+ singluar)
#{+ conj}
#(+ Substantiv im Plural)
# ... (/+ /)
#<">
# - literally, not a single quote
#eines Baukörpers/einer Maschine
#to take out/take down
#andere/r/s
# - et al.
#frühere(r/s)
# - et al.
#to chide {chided, chid; chided, chidden, chid} sb.
# - infix grammar annotations (for a simple verb; i.e. not in a phrase)
#"Are you a good singer/player?', 'I do moderately well.'
# - <"> or <'>
# ? general: allow both?
#   . <"> seems more frequent.
#(von Pilcher / Werktitel) ... :: ... (by Pilcher / work title)
# - frequent (with differing authors)
#Delegation {+Gen./bei}
#:)
#/A/cs/
#/Hg(CNO)2/ (Sprengstoff);
# - parsing question
#/sw, s/w/ .* /B&W, b&w, B/W/
#Ver­brau­cher­zen­t­ra­le {f} [tm] :: consumer advice centre [Br.]
# - a bunch of \u00AD contained (soft hyphens)
#   - Could be used to infer hyphenation.
# - []
#eine Abschrift beglaubigen {vt} [adm.] :: to certify; to attest; to exemplify [Am.] a copy
# ? prefixes in ()?
# ? During enrichment: Decuduce from {vt} or "to", that something is a collocate
#(formal)
#to get (oneself; sth.) to safety
#scanning | scanne$
#/s. and s.c./
#leise; ruhig; still {adj} | leiser; ruhiger; stiller | am leisesten; am ruhigsten; am stillsten
#name-dropping <name-drop> <name--dropping <namedrop>> <name--dropping <name drop>>
#das Grundgesetz  -- article
#Stahlpreisanstieg {m}; erhöhunh {f} :: increase in steel price; in the price of steel

# Hard to identify collocations (the part after the parens)
#  pressure (on sb.) to adapt/adjust
#  to mumble (away) to oneself

# Infer participle information
#  kollokieren; nebeneinanderstehen {vi} [ling.] | kollokierend; nebeneinanderstehend | kollokiert; nebeneinandergestanden :: to collocate | collocating | collocated

# <-> - annotations (<-> likely always surrounded by whitespace)
#  PS (lat. Postskriptum - post [nach] + scrivere [schreiben]) :: ps (Lat. postscript - post [after] + scrivere [writing])

# Racist.  TODO: Consider to annotate somehow.
#'Zehn kleine Negerlein'; 'Und dann gab's keines mehr' (von Christie / Werktitel) [lit.] :: 'Ten Little Niggers'; 'And Then There Were None' (by Christie / work title)


# Found when searching for units that contain a /./-expression:
#		Gesellschaft (des) bürgerlichen Rechts /GbR/; /GdbR/, BGB-Gesellschaft
#		emergency medical treatment room; emergency room [Am.] /e.r./; /ER/
#		 - questionable
#		Saint ...; /St/ [Br.]; /St./ [Am.]
#		 - questionable
#		cash with order /CWO/; /c.w.o./; cash in advance /CIA/
#		ante meridiem /a.m./; /am/
#		later; later on; /L8R/ [comp.]


# TODO: - '([A-Za-z]+ ){2}\{(m|f|n)\} *(\;|\|)'
#         - find badly located {}-annotations
#       - slashes (e.g. "er/sie hat/hatte bekommen")
#       - /+ *(Gen|Dat|Akk)/


# vi: noet
