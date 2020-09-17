{-
 - Language/TEI/ToXML/Header.hs - convert header to XML
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

module Language.TEI.ToXML.Header
  ( convHeader
  ) where


import Data.List (intercalate)
import Text.XML.Light

import qualified Config as Cfg
import Data.NatLang.Language
import Language.Ding.Syntax (Header(..))
import Language.TEI.ToXML.Aux
import Language.TEI.Version (makeVersion)


-- Notes:
--  * Whenever plain text cooccurs (on the same level) with other Content
--    inside an element (e.g. "<x>text<y>z</<y>more text</x>"), one should
--    use `mergeContent' on that list to avoid a pretty-printing like
--      <x>
--        text
--        <y>z</y>
--        more text
--      </x>
--   and instead cause everything to pe put in a single line.
-- * For information on how the header should look like, see the FreeDict
--   Wiki [0].
--  * See also: note on `unode' and `note' in ToXML/Body.hs.


plainRefNode :: String -> Element
plainRefNode tgt = unode "ref" (uattr "target" tgt, tgt)


-- | Convert the Ding (!) header straight to TEI XML.
convHeader :: Header -> Language -> Language -> Int -> Element
convHeader header srcLang tgtLang nHeadwords = teiHeader
 where

  ----------------------------------------------
  -- Information extracted from the Ding header

  -- Note:
  --  * The headerLicense field is not used.  Instead, the licensing
  --    information is hardcoded below, since the interaction with AGPL is
  --    non-trivial.

  dingVersion         = headerVersion header
  dingDate            = headerVersionDate header
  dingAuthor          = headerCopyrightHolder header
  dingCopyrightPeriod = headerCopyrightPeriod header
  dingURL             = headerURL header


  ---------------------------------------
  -- (Partly) non-structural information 

  -- Contains information customary to this particular dictionary..
  
  version = makeVersion dingVersion Cfg.modVersion

  title = show srcLang ++ " - " ++ show tgtLang
            ++ " Ding/" ++ Cfg.projectName ++ " dictionary"

  availability :: Element
  availability = unode "availability" $ (,) [uattr "status" "free"]
    [
      -- Copyright holders (Ding author & this program's contributors)
      unode "p" $ (++) "Copyright (C) " $
        intercalate ", " $ map (\ (p, y) -> p ++ " " ++ y) $
          (dingAuthor, dingCopyrightPeriod) : Cfg.contributors
    ,
      -- License notice
      unode "p" $ mergeContent
        [ text $ unwords
            [ "This dictionary is free software: you can redistribute it"
            , "and/or modify it under the terms of both the "
            ]
        , Elem $ unode "ref"
            ( uattr "target" Cfg.gpl3url
            , "GNU General Public License, version 3 (GPLv3)"
            )
        , text " and the "
        , Elem $ unode "ref"
            ( uattr "target" Cfg.agpl3url
            , "GNU Affero General Public License, version 3 (AGPLv3)"
            )
        , text $ unwords
            [ ", where each of these licenses applies to different parts of"
            , "this (combined) work."
            ]
        ]
    , unode "p" $ unwords
        [ "This dictionary is distributed in the hope that it will be useful,"
        , "but WITHOUT ANY WARRANTY; without even the implied warranty of"
        , "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
        , "GNU General Public License"
        , "and the"
        , "GNU Affero General Public License"
        , "for more details."
        ]
    , unode "p" $ mergeContent 
        [ text $ unwords
            [ "You should have received a copy of both the"
            , "GNU General Public License"
            , "and the"
            , "GNU Affero General Public License"
            , "along with this dictionary.  If not, see "
            ]
        , Elem $ plainRefNode "https://www.gnu.org/licenses/"
        , text "."
        ]
    , unode "p" $ mergeContent
        [ text $ unwords
            [ "The \"source form\", as defined in both the GPLv3 and AGPLv3,"
            , "of this dictionary is composed of the original"
            , "Ding dictionary (licensed "
            ]
        , Elem $ unode "ref" (uattr "target" Cfg.gpl2url, "GPLv2")
        , text $
            "+) and the " ++ Cfg.programName ++ " program (licensed AGPLv3+)."
        ]
    ]

  -- TODO: Add more notes.
  notesStmt :: Element
  notesStmt = unode "notesStmt" $
    [ unode "note" (uattr "type" "status", Cfg.status)
    ]

  sourceDesc :: Element
  sourceDesc = unode "sourceDesc"
    [ unode "p" $ mergeContent
        [ text "Home: "
        , Elem $ unode "ptr" $ uattr "target" dingURL
        ]
    , unode "p" $ mergeContent
        [ text "This dictionary was generated from the "
        , Elem $ unode "ref" (uattr "target" dingURL, "Ding")
        , text $ " dictionary, version " ++ dingVersion
            ++ " (" ++ dingDate ++ ") using the "
        , Elem $ unode "ref" (uattr "target" Cfg.programURL, Cfg.programName)
        , text " program."
        ]
    , unode "p" $ mergeContent
        [ text "The Ding dictionary can be obtained from "
        , Elem $ plainRefNode Cfg.dingDownloadURL
        , text "."
        ]
    , unode "p" $ mergeContent
        [ text $ "The " ++ Cfg.programName ++ " program can be obtained from "
        , Elem $ plainRefNode Cfg.programDownloadURL
        , text "."
        ]
    ]

  projectDesc :: Element
  projectDesc = unode "projectDesc" $ unode "p" $ mergeContent
    [ text $ unwords
        [ "This dictionary comes to you through nice people making it"
        , "available for free and for good."
        , "It is part of the", Cfg.projectName, "project, "
        ]
    , Elem $ plainRefNode Cfg.projectURL
    , text $ unwords
        [ "."
        , "This project aims to make many translating dictionaries available"
        , "for free."
        , "Your contributions are welcome!"
        ]
    ]


  ---------------------------------
  -- Purely structural information

  -- Code that only reflects some (TEI/FreeDict) rules & converntions.

  teiHeader :: Element
  teiHeader = unode "teiHeader" $ (,) [xmlLangAttr "en"] $
      fileDesc
    : encodingDesc
    : maybe [] pure revisionDesc

  fileDesc :: Element
  fileDesc = unode "fileDesc"
    [ titleStmt
    , editionStmt
    , unode "extent" $ show nHeadwords ++ " headwords"
    , publicationStmt
    , notesStmt
    , sourceDesc
    ]
  
  titleStmt :: Element
  titleStmt = unode "titleStmt"
    [ unode "title"  title
    , unode "author" dingAuthor
    , unode "editor" Cfg.editor
    , unode "respStmt"
        [ unode "resp" "Maintainer"
        , unode "name" Cfg.maintainer
        ]
    ]
  
  editionStmt :: Element
  editionStmt
    = unode "editionStmt"
    $ unode "edition"
    $ version
  
  publicationStmt :: Element
  publicationStmt = unode "publicationStmt"
    [ unode "publisher" Cfg.projectName
    , availability
    , unode "date" (uattr "when" Cfg.publicationDate, Cfg.publicationDate)
    , unode "pubPlace" $ unode "ref" Cfg.projectURL
    ]
  
  encodingDesc :: Element
  encodingDesc = unode "encodingDesc" projectDesc

  -- For reference, see eng-pol.tei and swh-eng.tei.
  revisionDesc :: Maybe Element
  revisionDesc
    | null Cfg.changes = Nothing
    | otherwise        = Just $ unode "revisionDesc"
                              $ map convChange Cfg.changes
   where
    convChange :: Cfg.Change -> Element
    convChange ch = unode "change"
      ( [ uattr "n"      chVersion
        , uattr "when" $ date
        , uattr "who"  $ unwords $ map ('#':) $ Cfg.chUsersShort ch
        ]
      , unode "list"
          ( [uattr "type" "bulleted"]
          , ( unode "head" $ mergeContent $
                [ text $ "Version " ++ chVersion ++ ", "
                , Elem $ unode "date" date
                , text ". Changes by "
                ] ++ nameList ++
                [ text "."
                ]
            )
            : map (unode "item") (Cfg.chItems ch)
          )
      )
     where
      chVersion = makeVersion (Cfg.chDingVersion ch) (Cfg.chModVersion ch)
      date      = Cfg.chDate ch

      nameList :: [Content]
      nameList = intercalate [text " and "]
               $ map (pure . Elem . unode "name") $ Cfg.chUsersFull ch


-- References
--  * [0] https://github.com/freedict/fd-dictionaries/wiki/FreeDict-HOWTO-%E2%80%93-Writing-Text-Encoding-Initiative-XML-Files

-- vi: ft=haskell ts=2 sw=2 et
