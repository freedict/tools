-- Language/Ding/AlexScanner.x - lexer
--
-- Copyright 2020 Einhard Leichtfuß
--
-- This file is part of ding2tei-haskell.
--
-- ding2tei-haskell is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published
-- by the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- ding2tei-haskell is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with ding2tei-haskell.  If not, see <https://www.gnu.org/licenses/>.


-- Notes:
--  * This file is supposed to be processed by Alex, "the lexical analyser
--    generator for Haskell".  It contains both Alex code and regular Haskell
--    code.  The result of Alex invocation is a regular Haskell source file.
--  * Alex User Guide: https://www.haskell.org/alex/#Documentation


{
-- Haskell module header and import statements.

module Language.Ding.AlexScanner (scan, AlexPosn) where

import qualified Data.Map.Strict as Map

import Language.Ding.Syntax.Grammar (grammarMap)
import Language.Ding.Token
}


%wrapper "posn"


-------------------------------------------------------------------------------
-- Definition of named character sets and regexes
-------------------------------------------------------------------------------

-- Notes:
--  * $x is a (named) character set
--  * @y is a (named) regular expression
--  * $x # $y is the set difference of $x and $y ($x - $y).
--  * See also: Alex User Guide, chapter 4.

$anyLeftBracket  = [ \{ \( \[ \< ]
$anyRightBracket = [ \} \) \] \> ]
$anyBracket = [ $anyLeftBracket $anyRightBracket ]

-- Any character that may (!) have special semantics (incl. <:>), excluding
-- $slashSpecialChar.
-- Note that <:> is only included to avoid matching <::> as part of @text.
-- Some characters, in particular all single colons have to be merged with
-- their surrounding (e.g., text atoms) later.
-- Similarly, <#> is only included to avoid initial <#> being matched as part
-- of @text.
$verySpecialChar = [ $anyBracket : \| \; \, \/ \~ \# ]

-- Characters that are only special in that they may occur enclosed in slashes.
$slashSpecialChar = [ \% \@ ]

$specialChar = [ $verySpecialChar $slashSpecialChar ]

$textChar = $printable # $specialChar

-- anything printable, not containing a special char; excluding surrounding
-- whitespace.
@text = [$textChar # $white] ($textChar* [$textChar # $white])?

-- This is a simple, restricted url pattern.  It is only supposed to match the
-- URL in the heading, as of now.
@url = https?:\/\/[A-Z a-z 0-9 _ \- \. \/]+

-- This must not contain '.' !
-- (Consider "/abbrev./".)
-- Note that most interpunctuation is usually followed by a space.
-- Note that $white includes '\n' (which is desired).
$freeSlashPre = [ $white $anyLeftBracket ]

-- This should in theory contain all interpunctuation (that does not require
-- preceding whitespace).
-- Note that $white includes '\n' (which is desired).
-- Expand as needed.
$freeSlashPost = [ $white $anyRightBracket \; \, : \. \? ! ]

-- Note: The final optional dot is redundant, as of now.
-- Note: This should not be used as generic word macro, as possibly desired
--       for <>.  (General mord macros should not allow a terminating '.'.)
-- TODO: $textChar should be replaced by something more sensible.
@slashWord = [$textChar # $white # \/]+ (\.|\?)?
@slashWordList = @slashWord ("/" @slashWord)+

-- There is only one known smiley, as of now.
@smiley = ":-)"


-------------------------------------------------------------------------------
-- Mapping from regexes to Haskell functions
-----------------------------------------------------------------------------

-- Notes:
--  * The functions are expected to take a matched string and produce a
--    corresponding token.
--  * Alex takes longest matching sequences (maximal munch).
--    * If several rules match the same longest sequence, the first one of them
--      applies.
--  * The lexer should always succeed.  Hence there is no special error
--    mechanism needed.
--    * This might change if the allowed whitespace characters are reduced.
--    * Also, if triple+ '/' should be caught at the lexer level.
--      * At the parser level, they'd be caught as successive '//', '/'.

tokens :-

  -- Header prefixes.  Should only match in the first few lines.
  ^ "# Version :: "       { regularToken $ const $ HeaderPrefix VersionPref }
  ^ "# Copyright (c) :: " { regularToken $ const $ HeaderPrefix CopyrightPref }
  ^ "# License :: "       { regularToken $ const $ HeaderPrefix LicensePref }
  ^ "# URL :: "           { regularToken $ const $ HeaderPrefix URLPref }

  @url                    { regularToken URL }


  -- Divide slashes into categories.
  -- Slashes have many roles (in particular one similar to brackets), but
  -- unfortunately there are no different opening and closing '/'-characters.
  -- Hence, infer such information from the context.
  -- See doc/syntax.slashes
  --  * (The description there is not strictly equivalent, but should be in all
  --    practical cases.)

  $freeSlashPre ^ "/" / $freeSlashPost  { regularToken $ const WeakSlash }
  $freeSlashPre ^ "/"                   { regularToken $ const OSlash }
                  "/" / "..."           { regularToken $ const StrongSlash }
                  "/" / $freeSlashPost  { regularToken $ const CSlash }
                  "/"                   { regularToken $ const StrongSlash }

  -- Treat single special characters between free slashes differently.
  -- This would typically be done in the parser, however it is difficult to
  -- identify such expressions using a CFG.
  -- Consider "+ / % / ~".  Since "word+ / word+ / word+" is generally
  -- permitted, this could not easily unambiguously parsed.  (Note that the
  -- "correct" parsing in this case is unclear.
  $freeSlashPre ^ "/ " $specialChar " /" / $freeSlashPost  {
    regularToken $ SlashSpecial . pure . (!!2)             }

  $freeSlashPre ^ "/ " @smiley " /" / $freeSlashPost       {
    regularToken $ Smiley . dropLast 2 . drop 2            }

  -- Treat stuff like "/ AC/DC /" also as a special case.  This is because,
  -- in general, strong slashes may occur in between weak slashes.  For this
  -- reason, this rule may actually incorrectly catch such cases.  However,
  -- the ambiguity needs to be taken care of somehow and this is the best
  -- way known to me.

  -- The dropLast part would be more efficient if the "monad" or
  -- "monadUserState" wrapper was used (one gets the length of the input).
  $freeSlashPre ^ "/ " @slashWordList " /" / $freeSlashPost {
    regularToken $ SlashExp . dropLast 2 . drop 2           }

  -- plural abbreviations
  $freeSlashPre ^ "/" @slashWord "/s" / $freeSlashPost      {
    regularToken $ SlashExpPlural . dropLast 2 . drop 1     }

  -- Double slashes are used (onserved once) to bind two adjacent alternative
  -- expressions (bound by a strong slash).
  -- Semantic: "a/b//c/d" -> (a or b) or (c or d)
  "//"                                  { regularToken Separator }

  -- Angle brackets are special in that they also may signify less- resp.
  -- greater-than.  Fortunately, they are, when brackets, always close to the
  -- enclosed object.  Treat similar to left and right slashes.
  -- TODO: Use separate context variables (freeAngle*) or rename freeSlash*.
  -- Important: This rule must be above the generic \<,\>-encompassing rule
  --            ($verySpecialChar).
  $freeSlashPre ^ [\< \>] / $freeSlashPost  { regularToken Text }

  \n                                    { regularToken $ const NL }
  \n\ +                                 { regularToken $ const LineCont }

  ::                                    { regularToken Separator }
  \<\>                                  { regularToken Separator }
  [$verySpecialChar # \/]               { regularToken Separator }

  -- Any character that is only special when (tightly) enclosed in '/', does
  -- not have any special meaning here.  Treat as common text.
  $slashSpecialChar                     { regularToken Text }

  @text                                 { regularToken kwOrText }

  [$white # \n]+                        { const Whitespace }


{
-------------------------------------------------------------------------------
-- Auxiliary Haskell code.
-------------------------------------------------------------------------------

-- | A simple token, as identified by Alex.
data SimpleToken = RegularToken Position Atom
                 | Whitespace String


-- | An action helper for regular tokens, to take care of the AlexPosn.
regularToken :: (String -> Atom) -> AlexPosn -> String -> SimpleToken
regularToken f p s = RegularToken (toPosition p) (f s)
 where
  toPosition :: AlexPosn -> Position
  toPosition (AlexPn _abs line col) = Position line col


-- Note: For now, only grammar keywords are recognized (TODO).
-- | Identify keyword from atomary string if applicable, or else simple text.
kwOrText :: String -> Atom
kwOrText s = case Map.lookup s grammarMap of
  Just gram -> Keyword $ GramKW gram
  Nothing   -> Text s

-- Notes
--  * There can never be two whitespace tokens in succession.
--  * One could use `foldr' here, but this is not very straightforward - the
--    folding function (in some cases) has to inspect the first element of its
--    list argument, which therefore is inspected twice (in such cases).

-- | Remove the whitespace from the token stream towards the respective
--   following regular token, as annotation.
mergeWS :: [SimpleToken] -> [Token]

mergeWS (                RegularToken pos atom : toks) =
  Token "" pos atom : mergeWS toks

mergeWS (Whitespace ws : RegularToken pos atom : toks) =
  Token ws pos atom : mergeWS toks

mergeWS (Whitespace _  : Whitespace _      : _)    =
  error "Language.Ding.AlexScanner: two successive whitespace tokens"

-- Ignore terminating whitespace.
mergeWS (Whitespace _  : [])                       = []
mergeWS []                                         = []


-- | Drop the given number of last elements.
dropLast :: Int -> [a] -> [a]
dropLast n xs = take (length xs - n) xs


-- | Convert a string into a stream of tokens.
scan :: String -> [Token]
scan = mergeWS . alexScanTokens
}

-- vi: ft=haskell ts=2 sw=2 et
