{-
 - Language/Ding/Token.hs - token structures as produced by the lexer
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


module Language.Ding.Token
 ( Token(..)
 , HeaderPrefix(..)
 , Position(..)
 , Atom(..)
 , Keyword(..)
 ) where

import qualified Data.Map.Strict as Map

import Text.ShowEssential (ShowEssential, showEssential)
import Language.Ding.Syntax.Grammar (GrammarAnnotation, grammarMapRev)


-- | Any keyword, excluding separators like <::>.
data Keyword = GramKW GrammarAnnotation
             -- ...
             | MultiKW String -- ^ Keyword with different potential semantics
 deriving Show


-- | Token, as produced by the lexer.  Annotated with any directly preceding
--   whitespace and the position in the input.
data Token = Token
               String     -- ^ Preceding whitespace.
               Position
               Atom
           | Empty        -- ^ Neutral element in the monoid.
 deriving Show


-- Note: One cannot simply use AlexPosn here, since this would introduce a
--       dependency cycle (in the current setup).

-- | Position of a token in the input, line and column.
data Position = Position Int Int
 deriving Show


-- TODO: Consider to add an explicit constructor for each possible separator.

-- | The essential part of a `Token'.
data Atom = NL
          | LineCont
          | WeakSlash
          | StrongSlash
          | OSlash
          | CSlash
--          | OBrace
--          | CBrace
--          | OParen
--          | CParen
--          | OBracket
--          | CBracket
--          | OAngle
--          | CAngle
--          | Semi
--          | Vert
--          | ...
          | SlashSpecial String
          | Smiley String
          | SlashExp String
          | SlashExpPlural String
          | Separator String    -- <,>, <|>, <::>, ...
          | Keyword Keyword
          | HeaderPrefix HeaderPrefix
          | URL String
          | Text String
 deriving Show


data HeaderPrefix = VersionPref
                  | CopyrightPref
                  | LicensePref
                  | URLPref
 deriving Show


instance ShowEssential Token where

  showEssential (Token _ _ atom) = showEssential atom
  showEssential Empty            = ""


-- TODO: link this with a future Pretty instance.
instance ShowEssential Atom where

  showEssential NL                 = "\n"
  showEssential LineCont           = "\n  "
  showEssential WeakSlash          = "/"
  showEssential StrongSlash        = "/"
  showEssential OSlash             = "/"
  showEssential CSlash             = "/"
  showEssential (SlashSpecial s)   = s   -- pretty: "/ " ++ s ++ " /"
  showEssential (Smiley s)         = s   -- pretty: "/ " ++ s ++ " /"
  showEssential (SlashExp s)       = s   -- pretty: "/ " ++ s ++ " /"
  showEssential (SlashExpPlural s) = s   -- pretty: "/" ++ s ++ "/s"
  showEssential (Separator s)      = s

  -- TODO: refine for further keyword types and decompose.
  showEssential (Keyword kw)       =
    case kw of
      GramKW kw' -> 
        case Map.lookup kw' grammarMapRev of
          Just s  -> s
          Nothing ->
            error "Language.Ding.Token: " ++ (show kw) ++ " not in map."
      MultiKW kw' -> kw'
  showEssential (HeaderPrefix _) =
    error "Language.Ding.Token: A header prefix token does not have a"
      ++ " useful string representation."
  showEssential (URL url)        = url
  showEssential (Text t)         = t


-- All tokens have a string representation, which, together with the preceding
-- whitespace identifies their value.  Two tokens may hence be combined in a
-- natural way, into a canonical `Text' token.

instance Semigroup Token where

  -- Join two tokens by concatenating their string representations, with the
  -- correct whitespace in between.
  (Token ws1 pos1 tok1) <> (Token ws2 _ tok2) =
    Token ws1 pos1 (Text $ showEssential tok1 ++ ws2 ++ showEssential tok2)

  -- Empty is supposed to be a neutral element.  Note that this means that
  -- `Empty <> tok' retains the whitespace from `tok'.  See also
  -- todo/parsing.elimination.
  Empty <> tok = tok
  tok <> Empty = tok


instance Monoid Token where

  -- The unit in the token monoid is the empty 'Text' with no preceding white-
  -- space.
  mempty = Empty


-- vi: ts=2 sw=2 et
