{-
 - Language/TEI/ToXML/Body.hs - convert TEI body to XML (AST)
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
 - Convert the TEI body to XML.
 -}
module Language.TEI.ToXML.Body (convBody) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (mapMaybe)
import Text.XML.Light

import Data.NatLang.Dictionary (Body(Body))
import Data.NatLang.Example (Example(..))
import Data.NatLang.InflectedForms (InflectedForms(..), InflectedForm(..))
import Data.NatLang.Language
import Data.NatLang.Usage (Usage(..))
import Language.TEI.Syntax
import Language.TEI.Syntax.Reference
import Language.TEI.ToXML.Aux
import Language.TEI.ToXML.Grammar
import Language.TEI.ToXML.Ident


-- Notes:
--  * The auxiliary functions are set up in the `where' block of the main
--    function so that they all have shared access to `srcLang' and `tgtLang'.
--    * Otherwise, these variables would need to be passed around a lot, even
--      to functions that do not use them directly but only pass them further
--      down.
--    * Some functions could be taken off the where block, but they are not,
--      for consistency.
--  * The functions `node _' and `unode _' are heavily overloaded.  They may
--    operate on e.g.:
--      ([Attr], String), (Attr, Element), ([Attr], [Element]), Attr.
--    * If a pair is provided, the left element is always the attribute (list)
--      and the right element the content.
--  * The @xml:lang attribute is placed
--    - on the top-level <text> element (source language), and
--    - on cit@type="trans" elements (target language).
--      - This includes translations of examples.
--    * Reasoning: All but translations are in the source language, this is
--      the minimal approach.
--      * Note that there are some fixed abbreviations (e.g., for grammatical
--        gender, or register), which violate this (usually English; TODO?).
--    * lg1-lg2.tei suggests otherwise.
--    * TEI Lex-0 suggests instead to add the attribute to <entry> and <cit>
--      elements.


-- | Convert the TEI Body to the corresponding TEI XML.
convBody :: Body Entry -> Language -> Language -> Element
convBody body srcLang tgtLang = convBody' body
 where

  -------------------------
  -- Some useful constants

  xmlLangSrc = xmlLangAttr $ showCode srcLang
  xmlLangTgt = xmlLangAttr $ showCode tgtLang


  ---------------------------
  -- The converter functions

  convBody' :: Body Entry -> Element
  convBody' (Body entries)
    = unode "text" $ (,) xmlLangSrc
    $ unode "body"
    $ map convEntry $ entries


  convEntry :: Entry -> Element
  convEntry e = unode "entry"
    ( [xmlIdAttr $ identToXMLId $ entryIdent e]
    ,   (convForm      $ entryForm    e)
      : maybe id (:)
        (convGrammar   $ entryGrammar e)
        (mapMaybe convSense $ entrySenses  e)
    )


  -- @type=lemma is a TEI Lex-0 recommendation.
  convForm :: Form -> Element
  convForm form = unode "form" $
       unode "orth" (formOrth form)
    :  map (convAbbrev "form") (formAbbrevs form)
    ++ maybe [] convInflectedForms (formInflected form)

  -- Encoded as suggested by Sebastian Humenda (on <freedict@freelists.org>,
  -- 2020-08-29).
  -- To be nested inside the main <form> element.
  -- Note: The tagName is "form" inside <form> and "cit" inside <cit>.
  convAbbrev :: String -> String -> Element
  convAbbrev tagName = unode tagName . (,) (uattr "type" "abbrev")
                     . unode "orth"


  -- tns[@value="past"]: no source - guessed / newly defined.
  -- tns[@value="pstp"] was found as an example in the TEI P5 doc (9.5.3.1).
  --  - TEI Lex-0 sug.: <gram type="participle">pap</gram>
  convInflectedForms :: InflectedForms -> [Element]
  convInflectedForms (InflectedForms (sp :| sps) (pp :| pps)) =
       map (convInflectedForm "past" (Just "indicative")) (sp : sps)
    ++ map (convInflectedForm "pstp" Nothing)             (pp : pps)

  -- Syntax:
  --  a) E-Mail from Sebastian Humenda (on <freedict@freelists.org>,
  --     2020-05-03)
  --     * Nest inside the main form.
  --  b) TEI Lex-0 (3.3): suggests value as content, e.g. <tns>pres</tns>.
  --  c) TEI P5
  --     * 9.3.1: grammar tags
  --     * <tns> doc: tns, mood example: <tns value="..."/>
  --  d) FreeDict TEI (@shumenda): Do not use @value, instead content.
  convInflectedForm :: String -> Maybe String -> InflectedForm -> Element
  convInflectedForm tense mMood (InflectedForm orth usages) = unode "form"
    ( [uattr "type" "infl"]
    ,   unode "gramGrp"
          (
              (unode "tns" $ tense)
            : maybe [] (pure . unode "mood") mMood
          )
      : unode "orth" orth
      : map convUsage usages
    )


  -- When there is no content, there needs not be no sense element.  Hence
  -- `Maybe Element'.
  convSense :: Sense -> Maybe Element
  convSense sense =
    let content = maybe id (:)
             (convGrammar         $ senseGrammar      sense)
          $  (map convUsage       $ senseUsages       sense)
          ++ (map convTranslation $ senseTranslations sense)
          ++ (map convExample     $ senseExamples     sense)
          ++ (map convRefGroup    $ senseReferences   sense)
          ++ (map (unode "note")  $ senseNotes        sense)
    in
      if null content
      then Nothing
      else Just $ unode "sense" content


  -- Note: inflected forms are dropped for now - unsure how to encode. (TODO)
  convTranslation :: Translation -> Element
  convTranslation trans = unode "cit"
    ( [uattr "type" "trans", xmlLangTgt]
    ,   unode "quote" (translationOrth trans)
      : (maybe id (:)
             (convGrammar            $ translationGrammar trans)
          $  (map convUsage          $ translationUsages trans)
          ++ (map (convAbbrev "cit") $ translationAbbrevs trans)
          ++ (map (unode "note")     $ translationNotes trans)
        )
    )


  convUsage :: Usage -> Element
  convUsage (Usage uType uStr) = unode "usg"
    ( uattr "type" (show uType)
    , uStr
    )


  convExample :: Example -> Element
  convExample (Example srcEx tgtExs) = unode "cit"
    ( [uattr "type" "example"]
    , unode "quote" srcEx : map convExTrans tgtExs
    )
   where
    convExTrans :: String -> Element
    convExTrans = unode "cit"   . (,) [uattr "type" "trans", xmlLangTgt]
                . unode "quote"


  convRefGroup :: ReferenceGroup -> Element
  convRefGroup (ReferenceGroup refType refs) = unode "xr"
    ( [uattr "type" (show refType)]
    , map convReference $ toList refs
    )
   where
    -- Notes:
    --  * Some references do not contain a link to another entry.
    --  * Such links need to be annotated with '#'.
    convReference :: Reference -> Element
    convReference (Reference mRef tgt) = unode "ref"
      ( maybe [] (pure . uattr "target" . ('#':) . identToXMLId) mRef
      , tgt
      )


-- vi: ft=haskell ts=2 sw=2 et
