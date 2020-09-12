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
 , Position(..)
 , Atom(..)
 , tokenToString
 ) where

import qualified Data.Map.Strict as Map

import Data.NatLang.Grammar (GramLexCategory)
import Language.Ding.Syntax.Grammar (grammarMapRev)



-- | Token, as produced by the lexer.  Annotated with any directly preceding
--   whitespace and the position in the input.
data Token
  = Token
      String     -- ^ Preceding whitespace.
      Position
      Atom
  | EmptyToken   -- ^ Neutral element in the monoid.
 deriving Show


-- Note: One cannot simply use AlexPosn here, since this would introduce a
--       dependency cycle (in the current setup).

-- | Position of a token in the input, line and column.
data Position = Position Int Int
 deriving Show


-- TODO: Consider to add an explicit constructor for each possible separator.

-- | The essential part of a `Token'.
data Atom = NL
          | LangSep       -- ^ "::"
          | Vert
          | Semi
          | Comma
          | Tilde
          | Plus
          | Wordswitch    -- ^ "<>"
          | StrongSlash   -- ^ see `Language.Ding.AlexScanner'
          | WeakSlash     -- ^ see `Language.Ding.AlexScanner'
          | DoubleSlash

          | OBrace
          | CBrace
          | OBracket
          | CBracket
          | OParen
          | CParen
          | OAngle
          | CAngle
          | OSlash        -- ^ see `Language.Ding.AlexScanner'
          | CSlash        -- ^ see `Language.Ding.AlexScanner'

          | SlashSpecial String
          | Smiley String
          | AbbrevWithSlash String
          | AbbrevPlural String
          | GramKW GramLexCategory
          | IntPronKW String
          -- | KW_multi      -- ^ several semantics, depending on context
          | Text String

          | HeaderLine String
 deriving Show


-- | Convert a token back to the string it represents excluding potential
--   delimiters and dropping the annotated preceding whitespace.  Uses
--   `atomToString'.
tokenToString :: Token -> String
tokenToString (Token _ _ atom) = atomToString atom
tokenToString EmptyToken       = ""


-- Note: Due to the loss of information on the kinds of slashes, the below
--       function should not used in a Show instance.
-- TODO: link this with a future Pretty instance.

-- | Convert an atom back to the string that it represents, excluding any
--   delimiters (</>).
--   This function is not injective, in particular the distinction of different
--   kinds of slashes is lost.
atomToString :: Atom -> String
atomToString NL                  = "\n"
atomToString LangSep             = "::"
atomToString Vert                = "|"
atomToString Semi                = ";"
atomToString Comma               = ","
atomToString Tilde               = "~"
atomToString Plus                = "+"
atomToString Wordswitch          = "<>"
atomToString StrongSlash         = "/"
atomToString WeakSlash           = "/"
atomToString DoubleSlash         = "//"

atomToString OBrace              = "{"
atomToString CBrace              = "}"
atomToString OBracket            = "["
atomToString CBracket            = "]"
atomToString OParen              = "("
atomToString CParen              = ")"
atomToString OAngle              = "<"
atomToString CAngle              = ">"
atomToString OSlash              = "/"
atomToString CSlash              = "/"

atomToString (SlashSpecial s)    = s   -- pretty: "/ " ++ s ++ " /"
atomToString (Smiley s)          = s   -- pretty: "/ " ++ s ++ " /"
atomToString (AbbrevWithSlash s) = s   -- pretty: "/ " ++ s ++ " /"
atomToString (AbbrevPlural s)    = s   -- pretty: "/" ++ s ++ "/s"
atomToString (GramKW gram)       =
  case Map.lookup gram grammarMapRev of
    Just s  -> s
    Nothing -> error "Language.Ding.Token: " ++ (show gram) ++ " not in map."
atomToString (IntPronKW pron)    = pron
atomToString (Text t)            = t

atomToString (HeaderLine l)      = l


-- All tokens have a string representation, which, together with the preceding
-- whitespace identifies their value.  Two tokens may hence be combined in a
-- natural way, into a canonical `Text' token.

instance Semigroup Token where

  -- Join two tokens by concatenating their string representations, with the
  -- correct whitespace in between.
  (Token ws1 pos1 atom1) <> (Token ws2 _ atom2) =
    Token ws1 pos1 (Text $ atomToString atom1 ++ ws2 ++ atomToString atom2)

  -- EmptyToken is supposed to be a neutral element.  Note that this means that
  -- `EmptyToken <> tok' retains the whitespace from `tok'.  See also
  -- todo/parsing.elimination.
  EmptyToken <> tok = tok
  tok <> EmptyToken = tok


instance Monoid Token where

  -- The unit in the token monoid is the empty 'Text' with no preceding white-
  -- space.
  mempty = EmptyToken


-- vi: ts=2 sw=2 et
