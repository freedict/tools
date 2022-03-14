{-
 - Config.hs - configuration of some output variables
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

{-
 - See also
 -  * Language/{Ding,TEI}/{Read,Show}/
 -  * Language/Ding/AlexScanner.x
 - on configuration and classification of usage and grammar annotatios.
 -}
module Config
  ( Person(..)
  , editor
  , maintainer
  , contributors
  , publicationDate
  , status
  , modVersion
  , programName
  , programURL
  , programDownloadURL
  , projectName
  , projectURL
  , dingDownloadURL
  , gpl3url
  , gpl2url
  , agpl3url
  , makeVersion
  , Change(..)
  , changes
  ) where


data Person = Person
  { personId   :: String
  , personName :: String
  }


-- Involved people:
person_shumenda :: Person
person_shumenda = Person "shumenda" "Sebastian Humenda"

person_eleichtfuss :: Person
person_eleichtfuss = Person "eleichtfuss" "Einhard Leichtfuß"


editor :: Person
editor = person_eleichtfuss

maintainer :: Person
maintainer = person_eleichtfuss

-- | Contributors to this program, with the corresponding copyright
--   periods
contributors :: [(Person, String)]
contributors =
  [ (person_eleichtfuss, "2020-2022")
  , (person_shumenda, "2021")
  ]

-- | Date of last output-affecting change of this program.
--   Must be given as YYYY-MM-DD.
publicationDate :: String
publicationDate = "2022-03-14"

status :: String
status = "stable"

-- | Modification version.  Should be incremented for any output-altering
--   change of this program.  To be reset, when a later version of the Ding
--   is targeted.
--   May also be set to <n>-devel to indicate frequent changes without version
--   change.
modVersion :: String
modVersion = "1"

programName :: String
programName = "ding2tei-haskell"

programURL :: String
programURL = "https://github.com/freedict/tools/tree/master/importers/ding2tei"

projectName :: String
projectName = "FreeDict"

projectURL :: String
projectURL = "https://freedict.org/"

programDownloadURL :: String
programDownloadURL = programURL

dingDownloadURL :: String
dingDownloadURL = "https://ftp.tu-chemnitz.de/pub/Local/urz/ding/"

gpl3url :: String
gpl3url = "https://www.gnu.org/licenses/gpl-3.0.html"

gpl2url :: String
gpl2url = "https://www.gnu.org/licenses/old-licenses/gpl-2.0.html"

agpl3url :: String
agpl3url = "https://www.gnu.org/licenses/agpl-3.0.html"


-- | Combine the Ding version and a modification version.
makeVersion :: String -> String -> String
makeVersion dingVer modVer = dingVer ++ "-fd" ++ modVer


-- Notes:
--  * chUsersShort and chUsersFull must be non-empty lists.
--    * The NonEmpty type is not used to ease manual modification.
data Change = Change
  { chDingVersion :: String
  , chModVersion  :: String
  , chPersons     :: [Person]
  , chDate        :: String   -- must be in YYYY-MM-DD format
  , chItems       :: [String] -- translated to <item>'s
  }


changes :: [Change]
changes =
  [ Change "1.8.1" "0.1" [person_eleichtfuss] "2020-09-11"
      [ "Initial import from Ding (version 1.8.1)."
      , "Fixed many syntax errors and inconsistencies in the Ding dictionary."
      , "Most explicit Ding annotations are transferred to TEI."
      , unwords
          [ "Several entries per Ding line are given references to one another"
          , "(@type=\"see\")."
          ]
      , unwords
          [ "Entries with more than one keyword on the source language side"
          , "are split up into multiple entries."
          , "These are linked using references of @type=\"syn\"."
          ]
      ]
  , Change "1.8.1" "0.2" [person_eleichtfuss] "2020-10-14"
      [ unwords
          [ "Identify examples to some entries; add the former to the latter"
          , "at 'entry/sense/cit[@type=\"example\"]' and remove the"
          , "examples from the list of regular entries."
          ]
      , unwords
          [ "Add annotations that are only implicitly present in the Ding"
          , "dictionary, such as: {f}, but no {pl} -> {noun}."
          ]
      , unwords
          [ "Transfer some annotations within Ding entries (e.g., when"
          , "\"Apfel\" is a masculine noun, we may infer that its translation,"
          , "\"apple\", is also a noun."
          ]
      , "Fixed some TEI syntax, as per comments from Sebastian Humenda."
      , "Recognize more types of annotations (notes)."
      ]
  , Change "1.8.1" "0.2.1" [person_eleichtfuss] "2020-10-28"
      [ "Mark units with annotated inflected forms as verbs."
      ]
  , Change "1.9" "1" [person_eleichtfuss] "2022-03-14"
      [ "Update to Ding version 1.9."
      ]
  ]


-- vi: ft=haskell ts=2 sw=2 et
