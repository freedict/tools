#!/usr/bin/env -S sed -Ef
#
# preprocess/main.sed - fix some irregularities in the Ding source
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

## Notes
#
# This script is used to fix some irregularities before handling processing to
# the main translating program.  Generally, only irregularities that are
# assumed as mistakes are fixed.
#
# Targeted at version 1.8.1 / 2016-09-06
#
# Called with `sed -E' (see Shebang); i.e. extended regular expressions are
# used.
#
# Conventions
# * Use the g (global) specifier, for anything that seems likely to possibly
#   occur elsewhere, even if it does not.  There might always be added another
#   occurrence in a later version and `g' does not cost much.
# * Be as generic as possible and as specific as necessary.
#   * Do not overgeneralize (e.g., "(n)" -> "{n}").  There may be legitimate
#     uses of apparent misspellings / bad syntax.
#     * Ex.: "to proffer sb. sth. /sth. to sb." -> "... sb. sth. / sth. to sb."
#       * Bad: "sb. /sth." -> "sb./sth." (when generalized).
# * Use groups (<()>) and back-references (<\$i>).
# * Use <`> as separator to avoid the necessity to escape (/).
# * Use "\<" and "\>" where applicable.
# * On changes of this script, diff the result with the former result to check
#   for erronous replacing.  This is usually doable in time.
# * When changing to another Ding version, review the diff (!) and check for
#   new irregularities (optional).
# * Ideally, when grouping (using parentheses), group in an understandable,
#   rather than an efficient way.
#   * Ex.: "s`\<(to take)/ (to carry out) (a measure)`\1 / \2 \3`g"
#     * Groups 2 and 3 could be merged.
#
# Helpful tools
# * Regexes (grep/$EDITOR/less) to find common mistakes
#   * e.g.: '\([^{]{0,10}\b(adj|adv|prp|n|m|f|...)\b' to find misusage of
#     parantheses (instead of braces).
# * util/extract_braceexps.(sed|sh)
#   * Analyze the output to find any irregular brace expression.


## Characters

# Quote characters from the Windows-1252 charset (convert to the Unicode
# version).  See <https://en.wikipedia.org/wiki/Windows-1252#Character_set>.
# The unicode values are 0x91 and 0x92 while the UTF-8 encodings are 0xC291 and
# 0xC292, respectively.
s`\xC2\x91`‘`g
s`\xC2\x92`’`g

# HTML code
s`\&#324\;`ń`g


## Grammar annotations

# Typo in grammatical gender
s`(Aggregationsreagenz) \{d\}`\1 \{n\}`g

# Wrong pos annotation
s`\<(makrohumiphag) \{m\} (\[zool\.\])`\1 \{adj\} \2`
# - (de); translates to "macrohumiphagous" (en), which is an adjective
#   - source: https://doi.org/10.1016/0378-1127(95)03580-4

# Parantheses instead of braces
s`\<(Felge|Glanzleinwand) \(f\)`\1 \{f\}`g
s`\<(Futterkattun) \(m\)`\1 \{m\}`g
s`\<(Normalnull) \(n\)`\1 \{n\}`g
s`\(adv\)`\{adv\}`g

s`\{pron\} \(relativ\)`{pron} {relativ}`g

# Consistent spacing and "dotting" with Gen/Dat/...
# Note: This could also be delegated to the parser.  Doing it here simplifies
#       matching keywords at the lexer stage, since '+' and '.' can be
#       considered part of the (unique) keyword.
s`\(\+ *(Gen|Akk|Dat)\.?\)`\{+\1.\}`g
s`\{\+ *(Gen|Akk|Dat)\.?\}`\{+\1.\}`g
s`\{\+ *(conj)\}`{+conj}`g

s`\[\+ Genitiv\]`{+Gen.}`g
s`\{(prp) *\+ *(Gen|Akk|Dat)\.\}`{\1; +\2.}`g

s`\<(die Summe) \+ Gen (\[math\.\])`\1 \{+Gen.\} \2`

# Incosistent use of separators between 'vi' and 'vt'.
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

s`\{(adj)\.\}`{\1}`g
s`\{prep\.\}`{prp}`g


## [] -> {}

s`\[(vi|pl)\]`\{\1\}`g
s`\[kein Plural\]`\{no pl\}`g

# Note: {no sing} never occurs, it is infered from {no pl} and {sing}.
s`\[only plural\]`{no sing}`g
s`\[no singular\]`{no sing}`g

s`(entsprechend etw\.) \[Dativ\]`\1 {+Dat.}`g

s`\[im Genitiv\]`{Gen.}`g


## [] -> ()

# TODO: Consider to catch this in the parser.
s`\((rarely used|rare)\)`[rare]`g


## []-annotations

# Note: There's probably more of these (TODO).
#  - alternatively, let the parser take care of it.
s`\[(Ös|Br|Am|Norddt|Mittelwestdt|Mitteldt|Dt|Bayr)\]`[\1.]`g

s`\[(Am) \.\]`[\1.]`g

s`\[(relig\.) und (Schw\.)\]`[\1, \2]`g

s`\[Scot.\]`[Sc.]`g
s`\[(NZ)\.\]`[\1]`g
s`\[New Zealand\]`[NZ]`g
s`\[Liecht\.\]`\[Lie.]`g
s`\[Sächsisch\]`[Sächs.]`g

# Debatable.  The replacement value occurs more frequently.
s`\[ZA\]`[South Africa]`g

# Debatable (Is there a semantic difference?).
# The replaced values occurs once, the replacing value 11 times.
s`\[Northern England\]`[Northern English]`g

# The reolacing value does not occur.  It just should be an englisch
# expression / abbreviation, in my opionion.  Analogous to [Northern English].
s`\[Nordirl\.\]`[Northern Irish]`g

# All of the below versions of [Lat.] appear exactly once.  Use [Lat.] as it
# seems to match the general convetion for dialects / languages the best.
s`\[(lateinisch|lat\.|Latin)\]`[Lat.]`g

s`\[(irisch|Irl\.)\]`[Ir\.]`g

s`\[(French) for (a female singer, especially in a nightclub)\]`[\1] (\2)`

s`\[(formal)/(Am\.)\]`[\1, \2]`g

# The only place where the separator is not a comma.
s`\[(archaic)\; (academic)\]`[\1, \2]`g


## Annotated conjugated forms

# Slashes instead of commata inside "{...}"
s`\{(chided)/(chid)\; (chided)/(chidden)/(chid\})`\{\1, \2\; \3, \4, \5\}`g
s`\{(got)\; (got)/(gotten \[Am\.\])\}`\{\1\; \2, \3\}`g
s`\{(outshined)/(outshone)\; (outshined)/(outshone)\}`\{\1, \2\; \3, \4\}`g

# Alternative form in parantheses instead of separated by comma
s`\{(swung) \((swang \[obs\.\])\)\; (swung)\}`\{\1, \2\; \3\}`g

# Do not allow multiple conjugation annotations.
# In this particular case it is also unclear (from the syntax only), which
# forms the [archaic]-annotations applies to.
s`\{(work)\; (worked)\} \{(wrought)\; (wrought) (\[archaic\])\}`\{\1, \3 \5; \2, \4 \5\}`

# <;> -> <,>
s`\{(besought, beseeched)\; (besought)\; (beseeched)\}`{\1\; \2, \3}`g
s`\{(awoke, awaked)\; (awoken)\; (awaked)\}`{\1\; \2, \3}`g
s`\{(pleaded)\; (pled \[coll\.\])\; (pleaded)\; (pled \[coll\.\])\}`{\1, \2; \3, \4}`g
s`\{(undergirded, undergirt)\; (undergirded)\; (undergirt)\}`{\1\; \2, \3}`g

# See: https://en.wiktionary.org/wiki/durst#English // 2020-09-09 00:34:20 CEST
s`\{(dared)\; (durst( \[obs\.\])?)\; (dared)\}`{\1, \2\; \4}`g

# <,> -> <;>
s`\{(hung), (hung)\}`{\1; \2}`g
s`\{(strung), (strung)\}`{\1; \2}`g
s`\{(kenned), (kent)\}`{\1; \2}`g


## Simple typos

# TODO: Check for effect.
#       - Use `t' command ?
# Incorrect lower case ('\b[a-zäöüß]+ \{(f|m|n|pl|no pl)\>.*::')
#                       (many false positives)
# Note that '\<' and '\>' do not generally suffice due to e.g. "...händler".
s` (ausfälle \{pl\}) ` \u\1`g
s` (instandhaltungseinschränkungen \{pl\})` \u\1`g
s` (tönung \{f\})` \u\1`g
s` (kleine) (auseinandersetzung \{f\})` \1 \u\2`g
s` (standringe \{pl\}) ` \u\1`g
s` (anreiz \{m\})` \u\1`g
s` (gleichrichten \{n\})` \u\1`g
s` (checksummen \{pl\})` \u\1`g

s`\<Parsely\>`Parsley`g

s`(^| )(o)(ertermittlungsspezialist(en)?) `\1W\3`g

s` au(fahrtsstraßen) (\{pl\})` Auf\1 \2`

s` e(ngepasstheit) ` A\1 `g
s` erhöhunh ` Erhöhung `g

s`\<(Kameraassistent) (in \{f\}|innen \{pl\})`\1\2`g

s`\<(an etw), (riechen)\>`\1. \2`

s`\<i(Buche)\>`\1`g

s`/(Jun\.\; jun\.)\; (Jnr)\; (Jr),/`/\1\; \2.\; \3./`g

# Only on English side a misspelling (though on the german side it likely
# should be capitalized).
s`( :: .*)\<ressources\>`\1resources`g

s`\[Nordestdt\.\]`[Nordosttdt.]`g
s`\[(Nordt\.|Norddtd\.|norddt\.|Nordddt\.)\]`[Norddt.]`g
s`\[Süddtd\.\]`[Süddt.]`g

s`\(teiweise\) (Änderung)\>`(teilweise) \1`g


# Superfluous <.> (if necessary, decided by count that the dotless variant is
# correct).
s`\[(sport|print|auto|slang|dated)\.\]`[\1]`g

# Missing <.>
s`\[(ugs|cook|naut|photo|econ|coll|humor|adm|fig|stud|fin|ornith|meteo|geh|zool|textil|techn|pol|envir|bot|telco|statist|soc|sci|poet|mil|med|ling|hist|aviat|chem|zool|adm|biochem)\]`[\1.]`g

# Superfluous < > before <.>.
s`\[(soc) \.\]`[\1.]`g

# Differently abbreviated annotations.
# Usually, the predominant version (by count) of two is taken.
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

# Not geogr. !
s`\<(Isogeotherme \{f\} ([A-Za-z ]+)) \[geo\.\]`\1 [geol.]`g

# This looks like [lit.], but it is not (infered from context).
# TODO: Consider to make literary its own annotation (i.e, do not "fix")?
s`\[literary\]`[poet.]`g

# [astr.] Could also mean [astrol.] but does not for all occurences.
s`\[(astr\.|aston\.|atron\.)\]`[astron.]`g

s`\[texti\.]`[textil.]`g
s`\[bichem\.\]`[biochem.]`g
s`\[colcoll\.\]`[coll.]`g
s`\[const\.\]`[constr.]`g

# Note that the 'or' is converted in alter.sed.
s`\[(geh\.|formal) or humorous\]`[\1 or humor.]`g
s`\[(formal)/humorous\]`[\1/humor.]`g

# Replacing value does not occur.  Adapt to common naming scheme.
s`\[arabisch\]`[Arab.]`g

# [] should conly contain well defined values, as opposed to ().
# Unfortunately, a little information gets lost this way.
# Too rare to consider in the parser.
s`\[(ugs\.) (schnelles Auto)\]`[\1] (\2)`g
s`\[(übtr\.) für (eine große, schlanke Person)\]`[\1] (\2)`g
s`\[(pej\.) für (Mittäter\; Gefolge)\]`[\1] (\2)`g
s`\[(obs\.) für (Küster, Kirchendiener)\]`[\1] (\2)`g

# Doubled [art], one of the with a superfluous <.>.
s`\[art\.\] (\[art\])`\1`g


## Abbreviations missing terminal <.>

s`\<(etw|jdm|jdn|jds|sth|sb)\>($|[^.])`\1.\2`g


## //-abbreviations

# Found by searching for "Abk" and "abbr".

s`\[(Abk|abbr)\.: ([A-Za-z.-]+)\]`/\2/`g
s`\((Abk|abbr)\.: ([A-Za-z.-]+)\)`/\2/`g

s`, (Abk|abbr)\.: ([A-Za-z.]+)\)`) /\2/`g

# Do not generalize this.  There may be abbreviations containing slashes.
s`\[Abk\. M\./M\]`/M.; M/`g

# Missing colon.
s`\[Abk\. (M\.\/M)\]`/ \1 /`g

s`(^|:: *)(\[sic\])`\1/\2/`g

s`\<(signed) /(sgd),/`\1 /\2./`g


## Misplaced {}-annotations

s`(\(Kfz:) \{n\} (/AND/\))`\1 \2`

# Guessing a little here.
s`/in \{prp\} \./`/in prp./`g


## Misplaced <::>

s`^(Hadaikum \{n\}\; Präarchaikum \{n\} \(Äon\) \[geol\.\]) (Hadean\; Pre-Archaean \[Br\.\]\; Pre-Archean \[Am\.\]) :: (\(eon\))$`\1 :: \2 \3`g


## Qustionably placed //-annotations

s`^(der Ältere)\; (Senior) /(d\.Ä\.); (Sen\.)/`\1 /\3/\; \2 /\4/`


## <...>

s`< (\[mus\.\]) >`\1`g

# The only "<>"-annotation syntactically clearly on a unit level.
# Since semantically wrong, fix as follows.
# See https://en.wiktionary.org/wiki/delt // 2020-09-06 20:39:00 CEST
s`\<(to deal) \{(dealt)\; (dealt)\} <(delt)>`\1 {\2; \3, \4 [archaic]}`g

# There is one single occurence of an `s' after "<>".  I see no reason for it.
# Also, the first english form should be in plural, too.
s`(Atomforscher \{pl\}\; Atomforscherinnen \{pl\}) :: (.*) \| (atomic scientist) <(nuclear scientist)>s$`\1 :: \2 \| \3s <\4s>`


## Missing entries
s`^(Betracht .*) :: *\|`\1 :: consideration |`


## Syntax-breaking typos (excl. slashes)
# Note: These are easy to find, hence there are more of them.

# This one is repeated, so possibly not a typo.
s`(supervised injection site) /SIS/Ms`\1s /SIS/s`
s`/SIS/M\>`/SIS/`g

s`( Kopenhagener Gebäck)>`\1`
s`<(420 \("four-twenty"\)) >`<\1>`g
s` (Bucuresti)>` <\1>`g
s`\((superlative of) -> (level)\)`(\1 ~\2)`g

s`(chid)\}\}`\1\}`


## Quotes
# Notes:
# * There is no clear distinction between single <'> and double <"> quotes.
#   Sometimes they are even used together (e.g. <"abc'>).
#   * TODO: It might be necessary to identify them.
# * Try to infer the "correct" quote variant from the surrounding.
# * <"> may also be used as abbreviation for the unit inch.
# * <'> may also be used in other contexts (e.g., <he'll>, <f'>).
#   * Usage as apostroph is quite frequent.

s`'(So für den Hausgebrauch)"`'\1'`
s`"(Are you a good singer/player\?)'`'\1'`
s`\<(unter dem Decknamen) '(George)"`\1 '\2'`g
s`'(echten Fruchtsaft)"`"\1"`
s`'(Pay per click)"`'\1'`g
s`"(And he'll believe you, will he\?)'`"\1"`g
s`"(The Loyal Subject)'`'\1'`g

s`"(Yeldon)" (und) "(Yelden),`"\1" \2 "\3",`
s`"(The Hunchback of Notre-Dame), (1831)`"\1", \2`g


## [/ -> ,]

# TODO: reconsider
#  - Note that the below listed do not catch some </> that are elsewhere
#    processed or created.
#s`\[(obs\.)/(humor\.)\]`[\1, \2]`g
#s`\[(humor\.)/(pej\.)\]`[\1, \2]`g
#s`\[(formal)/(humorous)\]`[\1, \2\]`g


## [.] -> /

# Note: Information on predominance is lost.
# TODO: reconsider
s`\<(table) \[(tabular)\] (spar)`\1/\2 \3`g
s`\<(ruby) \[(red)\]`\1/\2`g
s`\<(progressive) \[(prograde)\]`\1/\2`g
s`\<(pitch) \[(plunge)\]`\1/\2`g
s`\<(spheroidal) (jointing) \[(parting)\]`\1 \2/\3`g
s`\<(acial) \[(optic)\] (angle)`\1/\2 \3`g
s`\<(tight) \[(close)\] (sand)`\1/\2 \3`g
s`\<(cutting) \[(coal-cutter)\] (chain)`\1/\2 \3`g

# TODO: This is now really ugly.
s`\<(post) \[(nach|after)\]`\1/\2`g
s`\<(scrivere) \[(schreiben|writing)\]`\1/\2`g

s`\<(Jugendstil-) \[(Kunst)\]`\1/\2`g

# Ugly.
s`\(\[(im Preis)\] (enthalten)\)`((\1) \2)`g


## [.] -> (.)

# Optional words.  Information is lost.
# TODO: Consider to encode differently, e.g. [[]].

s`\<(eine Schar) \[(Personen)\]`\1 (\2)`g
s`\<(a clutch of) \[(persons)\]`\1 (\2)`g
s`\<(entlangrumpeln) \[(mit einem Fahrzeug)\]`\1 (\2)`g

# Unsure; possibly better translated to a slashed alternative.
s`\<(Brazilian) \[(optical)\] (pebble)\>`\1 (\2) \3`g

s`\<(to guzzle sth\.) \[(drink)\]`\1 (\2)`g
s`\<(to stitch) \[(book)\]`\1 (\2)`g

# Description.  Should become a <note>.
s`\[(ejecta\; discharges)\]`(\1)`g
s`\<(Sauerkohl \{m\}) \[(in einigen Regionen alternativer Begriff zu Sauerkraut)\]`\1 (\2)`g
s`\<(übertreffen \{vt\}) \[(in Geschwindigkeit oder Leistung)\]`\1 (\2)`g
s`\<(listicle) \[(list + article)\]`\1 (\2)`g
s`\<(eine Portion) \[(Mengenangabe)\]`\1 (\2)`g

# Racist.  TODO: Consider to annotate somehow.
s`\<(Zehn kleine Negerlein) \[(ein Kinderreim)\]`\1 (\2)`g
s`\<(Ten Little Indians) \[(a children's rhyme)\]`\1 (\2)`g


## Parenthese removal
# TODO: ? Solve in parser?
s`\; \((Abnutzung durch Reibung)\) (\[techn\.\]) ::`\; \1 \2 ::`g


## Superfluous <;>
s`\; *$``
s`\<(Verzögerungszeit \{f\})\; (\[electr\.\])`\1 \2`g


## Slashes
# See doc/syntax.slashes.
# TODO: Consider to generalize a few and in particular also catch wrongly
#       placed weak/strong slashes.

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


## Smileys
# Note: There is exactly one smiley.  I consider it better to enclose it in //.
#       This is therefore no fixing, it is an alteration of syntax.

s` (:-\))( |$)` / \1 /\2`g

# This is not a smiley.  Also possibly an alteration of syntax though.
s`\(@\)`/ @ /`g


## Mixed

# Superfluous semicolon; enrichment.
s`\<(note)\;\; (/N\.B\.\; NB/)`\1; nota bene [archaic] \2`

# slash, dot
s`\<(to look forward to sth\.)/ (to expect sth\.)\.`\1 / \2`g

# <;> -> <,>  ;  typo: see https://en.wiktionary.org/wiki/gird#Verb // 2020-09-03 00:47:30 CEST
s`\{(girded\, girt)\; (girded)\; git\}`{\1\; \2, girt}`g


## Wrong location of grammar annotation in phrase

s`\<(miteinander ins) (Bett) (gehen) (\{m\})`\1 \2 \4 \3`
# - Questionable.  See (1) below.

# Note: One might consider this syntactically correct with the semantics of an
#       annotation applying to the whole unit.  This is just a corner case
#       though.
s`:: (\{vt\}) (to pick) (a quarrel)\>`:: \2 \1 \3`


## Missing <;>

s`(Beide Schriftstücke sind online verfügbar\.) (Diese Schriftstücke sind beide online verfügbar\.)`\1; \2`


## Unbalanced parentheses

s`(to make a decision \(up\)on the documents before the court)\)`\1`


## Misc syntax errors

s`:: <> \|`:: |`g



## Further seemingly incorrect entries (TODO):

#ein gutes Blatt haben {n}							-- {} applies to part
#Freier, der den Autostrich abfährt {m} -- {} applies to whole / first part
#solange sie die Stelle innehat {n}     -- {} applies to ?
#Bouquet garni {n} cook									-- [cook.] ?
#Bemühen, das Gleiche zu erreichen {m}  -- {} applies to ?
#einen Unterhaltspfleger bestellen {m}  -- {} applies to part
# - alternative: disallow {} when refering to part of sentence (remove)    (1)
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

# Hard to identify collocations (the part after the parens)
#  pressure (on sb.) to adapt/adjust
#  to mumble (away) to oneself

# Infer participle information
#  kollokieren; nebeneinanderstehen {vi} [ling.] | kollokierend; nebeneinanderstehend | kollokiert; nebeneinandergestanden :: to collocate | collocating | collocated

# <-> - annotations (<-> likely always surrounded by whitespace)
#  PS (lat. Postskriptum - post [nach] + scrivere [schreiben]) :: ps (Lat. postscript - post [after] + scrivere [writing])


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
