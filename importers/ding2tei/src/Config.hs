{-
 - Config.hs - configuration of some output variables
 -}

module Config
  ( editor
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
  , Change(..)
  , changes
  ) where


editor :: String
editor = "Einhard Leichtfuß"

maintainer :: String
maintainer = editor

-- | Contributors to this program, with the corresponding copyright
--   periods
contributors :: [(String, String)]
contributors = [(editor, "2020")]

-- | Date of last output-affecting change of this program.
--   Must be given as YYYY-MM-DD.
publicationDate :: String
publicationDate = "2020-09-11"

status :: String
status = "stable"

-- | Modification version.  Should be incremented for any output-altering
--   change of this program.  To be reset, when a later version of the Ding
--   is targeted.
--   May also be set to <n>-devel to indicate frequent changes without version
--   change.
modVersion :: String
modVersion = "1-devel"

programName :: String
programName = "ding2tei-haskell"

programURL :: String
programURL = "https://github.com/freedict/tools/tree/ding2tei-haskell-rewrite/importers/ding2tei"

projectName :: String
projectName = "FreeDict"

projectURL :: String
projectURL = "http://freedict.org/"

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


-- Notes:
--  * chUsersShort and chUsersFull must be non-empty lists.
--    * The NonEmpty type is not used to ease manual modification.
data Change = Change
  { chDingVersion :: String
  , chModVersion  :: String
  , chUsersShort  :: [String]
  , chUsersFull   :: [String]
  , chDate        :: String   -- must be in YYYY-MM-DD format
  , chItems       :: [String] -- translated to <item>'s
  }


changes :: [Change]
changes =
  [ Change "1.8.1" "1-devel" ["eleichtfuss"] ["Einhard Leichtfuß"] "2020-09-11"
      [ "Initial import from Ding (version 1.8.1)."
      ]
  ]


-- vi: ft=haskell ts=2 sw=2 et
