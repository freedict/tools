{-
 - Language/TEI/ToXML.hs - convert TEI to XML (AST)
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

module Language.TEI.ToXML (prettyTEI) where

import Text.XML.Light

import Data.NatLang.Dictionary (Dictionary(..), Body(..))
import Language.TEI.Syntax (TEI)
import Language.TEI.ToXML.Aux
import Language.TEI.ToXML.Body
import Language.TEI.ToXML.Header


-- Notes:
--  * haskell-xml does not catch invalid input !
--    * `Language.TEI.ToXML.ValidateChar.validateString' should be applied
--      to the Ding input before attempting to convert to XML here.
--      * Currently done in Main.


-- | Pretty-print the TEI AST, as XML.
prettyTEI :: TEI -> String

-- Do not use `ppcTopElement' here, it adds a boring default <?xml ?> header.
prettyTEI tei =
  xmlDeclDoctype ++ (ppcElement prettyConfigPP $ teiToElement tei) ++ "\n"


-- | String form of the XML declaration and doctype (haskell-xml does not
--   support such properly).
xmlDeclDoctype :: String

-- TODO: Put the referenced files into place (e.g., by symlinking).
xmlDeclDoctype = unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>"
  , "<?xml-stylesheet type=\"text/xsl\" href=\"freedict-dictionary.xsl\"?>"
  , "<?xml-stylesheet type=\"text/css\" href=\"freedict-dictionary.css\"?>"
  , "<?oxygen SCHSchema=\"freedict-P5.rng\"?>"
  , "<?oxygen RNGSchema=\"freedict-P5.rng\" type=\"xml\"?>"
  , "<!DOCTYPE TEI SYSTEM \"freedict-P5.dtd\">"
  ]


-- | Translate a TEI AST to the corresponding XML AST.
teiToElement :: TEI -> Element
teiToElement tei = unode "TEI"
  ( [ uattr "xmlns" "http://www.tei-c.org/ns/1.0", uattr "version" "5.0" ]
  , [ convHeader (dictHeader tei) srcLang tgtLang nHeadwords
    , convBody   (dictBody tei)   srcLang tgtLang
    ]
  )
 where
  srcLang = dictSrcLang tei
  tgtLang = dictTgtLang tei

  -- TODO: nHeadwords should be calculated in the ding2tei conversion
  nHeadwords = length entries
   where
    (Body entries) = dictBody tei


-- vi: ft=haskell ts=2 sw=2 et
