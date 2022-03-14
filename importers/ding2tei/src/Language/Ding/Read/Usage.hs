{-
 - Language/Ding/Read/Usage.hs - read usage (<[]>) annotations from strings
 -
 - Copyright 2020,2022 Einhard Leichtfuß
 -
 - This file is part of ding2tei-haskell.
 -
 - ding2tei-haskell is free software: you can redistribute it and/or modify
 - it under the terms of the GNU Affero General Public License as published
 - by the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - ding2tei-haskell is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU Affero General Public License for more details.
 -
 - You should have received a copy of the GNU Affero General Public License
 - along with ding2tei-haskell.  If not, see <https://www.gnu.org/licenses/>.
 -}

{-|
 - Classify usage annoations into common TEI categories.
 -
 - Only a subset of all usage annotations are explicitly classified, all
 - others are assigned the default `hint' category.
 -}
module Language.Ding.Read.Usage
  ( readUsage
  ) where

import Data.NatLang.Usage

-- Notes:
--  * The lists below have been created with the help of
--     util/results/bracketexps.
--  * The meaning of the annotations (which type of annotation they are) has
--    been determined by searching examples in the Ding and infering from there
--    on (if not obvious).  In some cases, Wikipedia or the Wiktionary were of
--    help.
--  * See also Data/NatLang/Usage.hs and the notes there.


-- TODO: ? differentiate languages (English vs. German annotations)

-- Note: Words tend to be listed in decreasing order of frequency in the Ding.

-- TODO:
--  - "übtr."
--  - "eBr.", "eam."
--  - "mainly Am."
--  - "bes. Süddt."
--  - "Am., auch Br."
--  - "also Br."
--  - "alt"
--  - "pej."
--  - "obs."
--  - "Sprw." ~ "prov."
--  - "tm" (Trademark)
--  - "archaic", "dated"
--  - "also fig"
--  - "auch übtr."
--  - "humor."
--  - "Reitsport" ~ "riding"
--    - generally: German vs. English form
--  - "log"
--  - "especially Am."
--  - "academic" ~ "sci"? | register?
--  - "literary"
--  - ...


-- | Convert a usage string to a `Usage', thereby classifying it.
readUsage :: String -> Usage

-- Regions where German is spoken (in a particular way).
--  * Sometimes expressed as the corresponding dialect.
readUsage s@"Ös."            = Usage Regional s
readUsage s@"Schw."          = Usage Regional s
readUsage s@"Dt."            = Usage Regional s
readUsage s@"Bayr."          = Usage Regional s
readUsage s@"Norddt."        = Usage Regional s
readUsage s@"Süddt."         = Usage Regional s
readUsage s@"Mitteldt."      = Usage Regional s
readUsage s@"BW"             = Usage Regional s   -- Baden-Württemberg ?
readUsage s@"Westdt."        = Usage Regional s
readUsage s@"Ostdt."         = Usage Regional s
readUsage s@"Mittelwestdt."  = Usage Regional s
readUsage s@"Nordostdt."     = Usage Regional s
readUsage s@"Südtirol"       = Usage Regional s
readUsage s@"Nordwestdt."    = Usage Regional s
readUsage s@"Mittelostdt."   = Usage Regional s
readUsage s@"Südwestdt."     = Usage Regional s
readUsage s@"Lie."           = Usage Regional s
readUsage s@"Westös."        = Usage Regional s
readUsage s@"Lux."           = Usage Regional s
readUsage s@"Berlin"         = Usage Regional s
readUsage s@"Tirol"          = Usage Regional s
readUsage s@"Wien"           = Usage Regional s
readUsage s@"Ostös."         = Usage Regional s
readUsage s@"Oberdt."        = Usage Regional s
readUsage s@"Westfalen"      = Usage Regional s
readUsage s@"Sächs."         = Usage Regional s
readUsage s@"Pfalz"          = Usage Regional s
readUsage s@"Ostmitteldt."   = Usage Regional s
readUsage s@"Hessen"         = Usage Regional s
readUsage s@"Vbg."           = Usage Regional s   -- Vorarlbergisch
readUsage s@"Südostös."      = Usage Regional s
readUsage s@"Schwäb."        = Usage Regional s
readUsage s@"Rheinl."        = Usage Regional s
readUsage s@"DDR"            = Usage Regional s


-- Regions where English is spoken (in a particular way).
--  * Sometimes expressed as the corresponding dialect.
readUsage s@"Br."              = Usage Regional s
readUsage s@"Am."              = Usage Regional s
readUsage s@"Austr."           = Usage Regional s
readUsage s@"Sc."              = Usage Regional s
readUsage s@"NZ"               = Usage Regional s
readUsage s@"Ir."              = Usage Regional s
readUsage s@"Can."             = Usage Regional s
readUsage s@"In."              = Usage Regional s
readUsage s@"South Africa"     = Usage Regional s
readUsage s@"Welch"            = Usage Regional s
readUsage s@"SE Asia"          = Usage Regional s
readUsage s@"Northern English" = Usage Regional s
readUsage s@"Northern Irish"   = Usage Regional s


-- Languages being neither a dialect of German nor of English.
readUsage s@"French" = Usage Language s
readUsage s@"Ital."  = Usage Language s
readUsage s@"Lat."   = Usage Language s


-- Times
readUsage s@"archaic"      = Usage Time s
readUsage s@"altertümelnd" = Usage Time s   -- ~archaic
readUsage s@"altertümlich" = Usage Time s   -- ~archaic
readUsage s@"dated"        = Usage Time s


-- Registers (TODO: more)
--  * See https://en.wikipedia.org/wiki/Register_(sociolinguistics)
--    for why some of these are considered registers.
--  * TODO: Consider to map to ISO 12620.
readUsage s@"techn."   = Usage Register s   -- not a domain?
readUsage s@"ugs."     = Usage Register s   -- ~slang
readUsage s@"coll."    = Usage Register s   -- ~slang
readUsage s@"geh."     = Usage Register s   -- ? ~formal
readUsage s@"slang"    = Usage Register s
readUsage s@"formal"   = Usage Register s
readUsage s@"humor."   = Usage Register s   -- ?
readUsage s@"vulg."    = Usage Register s
readUsage s@"iron."    = Usage Register s
readUsage s@"fachspr." = Usage Register s   -- ~ "sci." ?
readUsage s@"euphem."  = Usage Register s   -- ?
readUsage s@"dialect"  = Usage Register s
readUsage s@"literary" = Usage Register s   -- ~ "poet."


-- Styles (TODO: more)
readUsage s@"fig." = Usage Style s


-- Preferences (none; TODO)
--  * TEI Guidelines: "preference level (‘chiefly’, ‘usually’, etc.)"
--  * TEI Lex-0: -> frequency: ex.: rare, occas.
--readUsage s@... = Usage Preference s


-- Acceptabilities (none)
--  * No further description in the TEI Guidelines.
--readUsage s@... = Usage Acceptability s


-- @type="textType", as in TEI Lex-0.  Currently considered a hint.
readUsage s@"jur."  = Usage Hint s  -- not a domain?
readUsage s@"adm."  = Usage Hint s  -- not a domain?
readUsage s@"poet." = Usage Hint s  -- not a register?;  ~ "literary"


-- Domains:
readUsage s@"ornith."   = Usage Domain s  -- the most frequent (!)
readUsage s@"med."      = Usage Domain s
readUsage s@"chem."     = Usage Domain s
readUsage s@"geogr."    = Usage Domain s
readUsage s@"bot."      = Usage Domain s
readUsage s@"zool."     = Usage Domain s
readUsage s@"comp."     = Usage Domain s
readUsage s@"fin."      = Usage Domain s
readUsage s@"electr."   = Usage Domain s
readUsage s@"cook."     = Usage Domain s
readUsage s@"econ."     = Usage Domain s
readUsage s@"min."      = Usage Domain s
readUsage s@"constr."   = Usage Domain s
readUsage s@"auto"      = Usage Domain s
readUsage s@"mil."      = Usage Domain s
readUsage s@"pol."      = Usage Domain s
readUsage s@"anat."     = Usage Domain s
readUsage s@"mach."     = Usage Domain s
readUsage s@"sport"     = Usage Domain s
readUsage s@"soc."      = Usage Domain s
readUsage s@"mus."      = Usage Domain s
readUsage s@"textil."   = Usage Domain s
readUsage s@"geol."     = Usage Domain s
readUsage s@"math."     = Usage Domain s
readUsage s@"agr."      = Usage Domain s
readUsage s@"hist."     = Usage Domain s
readUsage s@"phys."     = Usage Domain s
readUsage s@"relig."    = Usage Domain s
readUsage s@"biol."     = Usage Domain s
readUsage s@"naut."     = Usage Domain s
readUsage s@"aviat."    = Usage Domain s
readUsage s@"ling."     = Usage Domain s
readUsage s@"envir."    = Usage Domain s
readUsage s@"telco."    = Usage Domain s
readUsage s@"lit."      = Usage Domain s  -- "literature", not "literally"
readUsage s@"psych."    = Usage Domain s
readUsage s@"biochem."  = Usage Domain s
readUsage s@"art"       = Usage Domain s
readUsage s@"school"    = Usage Domain s
readUsage s@"meteo."    = Usage Domain s
readUsage s@"transp."   = Usage Domain s
readUsage s@"arch."     = Usage Domain s  -- "architecture"
readUsage s@"astron."   = Usage Domain s
readUsage s@"print"     = Usage Domain s
readUsage s@"pharm."    = Usage Domain s
readUsage s@"stud."     = Usage Domain s
readUsage s@"phil."     = Usage Domain s
readUsage s@"statist."  = Usage Domain s
readUsage s@"photo."    = Usage Domain s
readUsage s@"astrol."   = Usage Domain s
readUsage s@"phot."     = Usage Domain s
readUsage s@"theat."    = Usage Domain s
readUsage s@"mech."     = Usage Domain s
readUsage s@"laser."    = Usage Domain s
readUsage s@"insur."    = Usage Domain s

-- does not really fit the common syntax:
readUsage s@"Beleuchtungstechnik" = Usage Domain s

readUsage s@"vetmed."   = Usage Domain s
readUsage s@"TV"        = Usage Domain s
readUsage s@"riding"    = Usage Domain s
readUsage s@"Reitsport" = Usage Domain s
readUsage s@"Radsport"  = Usage Domain s
readUsage s@"cycling"   = Usage Domain s
readUsage s@"archeol."  = Usage Domain s -- different from "arch."


-- Default catchall
readUsage s = Usage Hint s


-- vi: ft=haskell ts=2 sw=2 et
