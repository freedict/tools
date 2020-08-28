{-
 - Language/Ding/Syntax/Usage.hs - usage (<[]>) annotations
 -
 - Copyright 2020 Einhard Leichtfuß
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

module Language.Ding.Syntax.Usage () where

--
-- Notes:
--  * The lists below have been created with the help of
--     util/results/bracketexps.
--  * The meaning of the annotations (which type of annotation they are) has
--    been determined by searching examples in the Ding and infering from there
--    on (if not obvious).  In some cases, Wikipedia or the Wiktionary were of
--    help.
--  * See the TEI Lex-0 documentation for <usg> types.
--    * https://dariah-eric.github.io/lexicalresources/pages/TEILex0/TEILex0.html#index.xml-body.1_div.7_div.2
--  * See also the Wikipedia on Varieties and Registers (contains a list).
--    * https://en.wikipedia.org/wiki/Variety_(linguistics)
--    * https://en.wikipedia.org/wiki/Register_(sociolinguistics)
--

data Variety = Dialect String
             | Register String

data Domain = Domain String


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
--  - ...

-- TODO: Consider to separate different languages (French, Latin, ...).
-- | Dialects (of English and German) and languages, as annotated in the Ding.
dialects :: [String]
dialects =
  [ "Br."
  , "Am."
  , "Ös."
  , "Schw."
  , "Dt."
  , "Bayr."
  , "Norddt."
  , "Süddt."
  , "Austr."
  , "Mitteldt."
  , "BW"          -- Not sure what dialect this refers to (Baden-Württemberg?).
  , "Sc."
  , "NZ"
  , "Ir."
  , "Westdt."
  , "Ostdt."
  , "Mittelwestdt."
  , "Nordostdt."
  , "Südtirol"
  , "Nordwestdt."
  , "Mittelostdt."
  , "Can."
  , "Südwestdt."
  , "Lie."
  , "Northern English"
  , "Westös."
  , "Lux."
  , "In."
  , "Berlin"
  , "South Africa"
  , "Tirol"
  , "French"
  , "Wien"
  , "Ostös."
  , "Oberdt."
  , "Westfalen"
  , "Sächs."
  , "Pfalz"
  , "Ostmitteldt."
  , "Ital."
  , "Hessen"
  , "Welch"
  , "Vbg."        -- Vorarlbergisch
  , "Südostös."
  , "SE Asia"
  , "Schwäb."
  , "Rheinl."
  , "Northern Irish"
  , "Lat."
  , "DDR"
  ]

-- See https://en.wikipedia.org/wiki/Register_(sociolinguistics)
-- for why some of these are considered registers.
-- TODO: Consider to map to ISO 12620.
registers :: [String]
registers =
  [ "techn."  -- might also be considered a usage domain
  , "ugs."    -- ~slang
  , "coll."   -- ~slang
  , "geh."    -- ? ~formal
  , "slang"
  , "formal"
  , "humor."  -- ?
  , "vulg."
  , "iron."
  , "fachspr."    -- ~ "sci." ?
  , "euphem." -- ?
  , "dialect"
  -- TODO: more
  ]

-- Maps to <usg type="textType"/> in TEI Lex-0.
textTypes :: [String]
textTypes =
  [ "jur."    -- not a domain?
  , "adm."    -- not a domain?
  , "poet."   -- not a register?
  ]

domains :: [String]
domains =
  [ "ornith."   -- the most frequent (!)
  , "med."
  , "chem."
  , "geogr."
  , "bot."
  , "zool."
  , "comp."
  , "fin."
  , "electr."
  , "cook."
  , "econ."
  , "min."
  , "constr."
  , "auto"
  , "mil."
  , "pol."
  , "anat."
  , "mach."
  , "sport"
  , "soc."
  , "mus."
  , "textil."
  , "geol."
  , "math."
  , "agr."
  , "hist."
  , "phys."
  , "relig."
  , "biol."
  , "naut."
  , "aviat."
  , "ling."
  , "envir."
  , "telco."
  , "lit."
  , "psych."
  , "biochem."
  , "art"
  , "school"
  , "meteo."
  , "transp."
  , "arch."
  , "astron."
  , "print"
  , "pharm."
  , "stud."
  , "phil."
  , "statist."
  , "photo."
  , "astrol."
  , "phot."
  , "theat."
  , "mech."
  , "laser."
  , "insur."
  , "Beleuchtungstechnik"   -- does not really fit the common syntax
  , "vetmed."
  , "TV"
  , "riding"
  , "Reitsport"
  , "Radsport"
  , "cycling"
  ]

-- vi: ts=2 sw=2 et
