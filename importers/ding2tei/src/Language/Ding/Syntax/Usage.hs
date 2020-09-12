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

module Language.Ding.Syntax.Usage
  ( stringToUsage
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

import Data.NatLang.Usage

-- Notes:
--  * The lists below have been created with the help of
--     util/results/bracketexps.
--  * The meaning of the annotations (which type of annotation they are) has
--    been determined by searching examples in the Ding and infering from there
--    on (if not obvious).  In some cases, Wikipedia or the Wiktionary were of
--    help.
--  * See also Data/NatLang/Usage.hs and the notes there.


stringToUsage :: String -> Usage
stringToUsage s = Usage (fromMaybe Hint $ Map.lookup s usageMap) s


-- TODO: ? differentiate languages (English vs. German annotations)
usageMap :: Map String UsageType
usageMap = Map.fromList $
     map (flip (,) Regional) (germanRegions ++ englishRegions)
  ++ map (flip (,) Time) times
  ++ map (flip (,) Domain) domains
  ++ map (flip (,) Register) registers
  ++ map (flip (,) Style) styles
  ++ map (flip (,) Preference) preferences
  ++ map (flip (,) Acceptability) acceptabilities
  ++ map (flip (,) Language) foreignLanguages


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

-- | Regions where German is spoken (in a particular way).
--   Sometimes expressed as the corresponding dialect.
germanRegions :: [String]
germanRegions =
  [ "Ös."
  , "Schw."
  , "Dt."
  , "Bayr."
  , "Norddt."
  , "Süddt."
  , "Mitteldt."
  , "BW"          -- Not sure what dialect this refers to (Baden-Württemberg?).
  , "Westdt."
  , "Ostdt."
  , "Mittelwestdt."
  , "Nordostdt."
  , "Südtirol"
  , "Nordwestdt."
  , "Mittelostdt."
  , "Südwestdt."
  , "Lie."
  , "Westös."
  , "Lux."
  , "Berlin"
  , "Tirol"
  , "Wien"
  , "Ostös."
  , "Oberdt."
  , "Westfalen"
  , "Sächs."
  , "Pfalz"
  , "Ostmitteldt."
  , "Hessen"
  , "Vbg."        -- Vorarlbergisch
  , "Südostös."
  , "Schwäb."
  , "Rheinl."
  , "DDR"
  ]

-- | Regions where English is spoken (in a particular way).
--   Sometimes expressed as the corresponding dialect.
englishRegions :: [String]
englishRegions =
  [ "Br."
  , "Am."
  , "Austr."
  , "Sc."
  , "NZ"
  , "Ir."
  , "Can."
  , "In."
  , "South Africa"
  , "Welch"
  , "SE Asia"
  , "Northern English"
  , "Northern Irish"
  ]

-- | Languages being neither a dialect of German nor of English.
foreignLanguages :: [String]
foreignLanguages =
  [ "French"
  , "Ital."
  , "Lat."
  ]


times :: [String]
times =
  [ "archaic"
  , "altertümelnd"    -- ~archaic
  , "altertümlich"    -- ~archaic
  , "dated"
  -- ...
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

styles :: [String]
styles =
  [ "fig."
  , "lit."
  -- ...
  ]

-- - TEI doc: "preference level (‘chiefly’, ‘usually’, etc.)"
-- - TEI Lex-0: -> frequency: ex.: rare, occas.
preferences :: [String]
preferences = []

-- No further description in TEI doc.
acceptabilities :: [String]
acceptabilities = []
  

-- TODO: Not used.
--  - This is a TEI Lex-0 category, that is not among the suggested ones for
--    TEI.
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
  , "archeol."    -- different from "arch."
  ]

-- vi: ts=2 sw=2 et
