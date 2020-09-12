{-
 - Language/Ding/Parser.hs - parser
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

module Language.Ding.Parser (parse) where

import Data.NatLang.Dictionary (Dictionary(Dictionary), Body(Body))
import Data.NatLang.Language (Language(German, English))
import Language.Ding.Parser.Header (parseHeader)
import Language.Ding.Parser.Line (parseLine)
import Language.Ding.Syntax (Ding)
import Language.Ding.Token (Token(..), Atom(..))


-- | Construct a Ding AST from a list of tokens.
parse :: [Token] -> Ding
parse ts =
  --let (headerLines, bodyLines) = separateHeaderLines $ tokLines ts
  --in  Ding (parseHeader headerLines) (map parseLine bodyLines)
  let (headerLines, bodyToks) = separateHeaderLines ts
  in  Dictionary
        (parseHeader headerLines)
        German
        English
        (Body $ map parseLine $ tokLines bodyToks)


-- | Separate the initial lines belonging to the header from the body lines.
--   Header lines are converted to strings.
separateHeaderLines :: [Token] -> ([String], [Token])
separateHeaderLines (Token _ _ (HeaderLine hl) : Token _ _ NL : tls) =
  let (hls, tls') = separateHeaderLines tls
  in  (hl : hls, tls')
separateHeaderLines tls                                   = ([], tls)


-- | Break a list of tokens into a list of lines, the latter being also a list
--   of tokens.  The separating newline tokens are retained.  The list of
--   tokens is expected to be newline-terminated.
--   Similar to `Data.OldList.lines', but on tokens, with separator
--   `Language.Ding.Token.NL'.
tokLines :: [Token] -> [[Token]]
tokLines ts = case breakLine ts of
  (l, [])  -> l : []
  (l, ts') -> l : tokLines ts'


-- | Separate a single line, including newline token from the stream.
--   Requires a newline token to be present.
breakLine :: [Token] -> ([Token], [Token])
breakLine (nl@(Token _ _ NL) : ts) = ([nl], ts)
breakLine (t                 : ts) =
  let (l, ts') = breakLine ts
  in  (t : l, ts')
breakLine []                       =
  error "Input is not newline-terminated."


-- vi: ts=2 sw=2 et
