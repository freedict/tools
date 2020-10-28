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


import Data.NatLang.Grammar
import Language.Ding.Token
}


-- In contrast to the "basic" wrapper, "posn" allows for keeping track of
-- positions.
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
-- Note that <:> is only included to avoid matching <::> as part of @word.
-- Some characters, in particular all single colons have to be merged with
-- their surrounding (e.g., text atoms) later.
$specialChar = [ $anyBracket : \| \; \, \/ \~ \+ ]

-- Characters that are special in that they may occur enclosed in slashes.
$slashSpecialChar = [ $specialChar \% \@ ]

$textChar = $printable # [$specialChar $white]

-- Anything printable, not containing a special char, nor whitespace.
@word = $textChar+

-- To allow infix whitespace, use the below (in place of @word):
-- @text = [$textChar # $white] ($textChar* [$textChar # $white])?

-- A left context that marks a slash as "left-free".  This is used
-- particularly to identify "opening" and "closing" slashes, where the
-- former is "left-free", but not "right-free", as for example in
--  /abbrev./
-- Notes:
--  * This must not contain '.' !  (consider /abbrev./)
--  * Most interpunctuation is usually followed by a space.
--  * $white includes '\n' (which is desired).
$slashLeftFree = [ $white $anyLeftBracket ]

-- A right context that marks a slash as "right-free" (see just above).
-- This should in theory contain all interpunctuation (that does not require
-- preceding whitespace).
-- Note:
--  * Expand the interpunctuation character set as needed.
$slashRightFree = [ $white $anyRightBracket \; \, : \. \? ! ]

-- A non-empty sequence of slash-separated words (@slashWordList), which forms
-- a special case within slashes.
-- Notes:
--  * The final optional dot is redundant, as of now.
--  * This should not be used as generic word macro, as possibly desired for
--    "<>".  (General mord macros should not allow a terminating '.'.)
--  * $textChar should possibly be replaced by something more sensible.
@slashWord = [$textChar # \/]+ (\.|\?)?
@slashWordList = @slashWord ("/" @slashWord)+

-- There is only one known smiley, as of now.
-- Smileys are treated as abbreviations.
@smiley = ":-)"

$word_end = ~$textChar


-------------------------------------------------------------------------------
-- Mapping from regexes to Haskell functions (actions)
-------------------------------------------------------------------------------

-- Notes:
--  * The functions are expected to take a matched string and produce a
--    corresponding token.
--    * Because the "posn" wrapper is used (instead of "basic"), most such
--      functions have to be wrapped in `regularToken'.
--  * Alex takes longest matching sequences (maximal munch).
--    * If several rules match the same longest sequence, the first one of them
--      applies.
--  * The lexer should always succeed.  Hence there is no special error
--    mechanism needed.
--    * This might change if the allowed whitespace characters are reduced.
--    * Also, if triple+ '/' should be caught at the lexer level.
--      * At the parser level, they'd be caught as successive '//', '/'.

tokens :-

  -- A header line.  To be parsed as is by Language.Ding.Parser.Header.
  ^ \# .* $                               { regularToken HeaderLine }


  ---------------------------------------------------------------------------
  -- Separators (excl. slashes and single angle brackets)

  \n                        { regularToken $ const NL }

  ::                        { regularToken $ const LangSep }
  \|                        { regularToken $ const Vert }
  \;                        { regularToken $ const Semi }
  \,                        { regularToken $ const Comma }
  \~                        { regularToken $ const Tilde }
  \+                        { regularToken $ const Plus }
  \<\>                      { regularToken $ const Wordswitch }

  \{                        { regularToken $ const OBrace }
  \}                        { regularToken $ const CBrace }
  \[                        { regularToken $ const OBracket }
  \]                        { regularToken $ const CBracket }
  \(                        { regularToken $ const OParen }
  \)                        { regularToken $ const CParen }


  -- This is only caught explicitly, since it may not be caught as part of
  -- @word (which could be changed, by complicating the regex--<::> would
  -- still need to be excluded).
  :                         { regularToken Text }


  ---------------------------------------------------------------------------
  -- Slashes

  -- See doc/syntax.slashes

  -- Divide slashes into categories.
  -- Slashes have many roles (in particular one similar to brackets), but
  -- unfortunately there are no different opening and closing '/'-characters.
  -- Hence, infer such information from the context.
  --  * (The description there is not strictly equivalent, but should be in all
  --    practical cases.)

  $slashLeftFree ^ "/" / $slashRightFree  { regularToken $ const WeakSlash }
  $slashLeftFree ^ "/"                    { regularToken $ const OSlash }
                   "/" / "..."            { regularToken $ const StrongSlash }
                   "/" / $slashRightFree  { regularToken $ const CSlash }
                   "/"                    { regularToken $ const StrongSlash }

  -- Treat single special characters between free slashes differently.
  -- This would typically be done in the parser, however it is difficult to
  -- identify such expressions using a CFG.
  -- Consider "+ / % / ~".  Since "word+ / word+ / word+" is generally
  -- permitted, this could not easily unambiguously parsed.  (Note that the
  -- "correct" parsing in this case is unclear.
  $slashLeftFree ^ "/ " $specialChar " /" / $slashRightFree {
    regularToken $ SlashSpecial . pure . (!!2)              }

  $slashLeftFree ^ "/ " @smiley " /" / $slashRightFree      {
    regularToken $ Abbrev . dropLast 2 . drop 2             }

  -- Treat stuff like "/ AC/DC /" also as a special case.  This is because,
  -- in general, strong slashes may occur in between weak slashes.  For this
  -- reason, this rule may actually incorrectly catch such cases.  However,
  -- the ambiguity needs to be taken care of somehow and this is the best
  -- way known to me.

  -- The dropLast part would be more efficient if the "monad" or
  -- "monadUserState" wrapper was used (one gets the length of the input).
  $slashLeftFree ^ "/ " @slashWordList " /" / $slashRightFree {
    regularToken $ Abbrev . dropLast 2 . drop 2               }

  -- An awkward special case that cannot (easily) be handled otherwise:
  $white ^ \"\/\.ed\" / $white                            {
    regularToken $ Abbrev . dropLast 1 . drop 1           }
  -- "

  -- Plural abbreviations.
  -- Note: No more than one word between the slashes is permitted.
  --       Otherwise, lexing + parsing would become more difficult.
  $slashLeftFree ^ "/" @slashWord "/s" / $slashRightFree  {
    regularToken $ AbbrevPlural . dropLast 2 . drop 1     }

  -- Double slashes are used (onserved once) to bind two adjacent alternative
  -- expressions (bound by a strong slash).
  -- Semantic: "a/b//c/d" -> (a or b) or (c or d)
  "//"                                  { regularToken $ const DoubleSlash }


  ---------------------------------------------------------------------------
  -- Angle brackets

  -- Angle brackets are special in that they also may signify less- resp.
  -- greater-than.  Fortunately, they are, when brackets, always close to the
  -- enclosed object.  Treat similar to left and right slashes.
  -- Important: This rule must be above the generic \< and \> rule(s).
  $slashLeftFree ^ [\< \>] / $slashRightFree  { regularToken Text }

  \<                                          { regularToken $ const OAngle }
  \>                                          { regularToken $ const CAngle }


  ---------------------------------------------------------------------------
  -- Keywords

  -- Note:
  --  * Multi-word keywords need to be given the right context $word_end.
  --    * This ensures that the word ends there.
  --    * Ex.: Let "a b" be a multi-word keyword, and "a bc " at the start of
  --      the character stream.  This should be matched as two text tokens, "a"
  --      and "bc", not as keyword "a b" and text "c".

  "to"                      { regularToken $ const KW_to }

  -- Grammar keywords
  "f"                       { gramKW $ Gender Feminine }
  "m"                       { gramKW $ Gender Masculine }
  "n"                       { gramKW $ Gender Neuter }
  "sing"                    { gramKW $ Number $ Singular False }
  "pl"                      { gramKW $ Number $ Plural   False }
  "no pl"   / $word_end     { gramKW $ Number $ Singular True }
  "no sing" / $word_end     { gramKW $ Number $ Plural   True }
  --
  "v"                       { gramKW $ PartOfSpeech $ Verb [] }
  "vt"                      { gramKW $ PartOfSpeech $ Verb [Transitive] }
  "vi"                      { gramKW $ PartOfSpeech $ Verb [Intransitive] }
  "vr"                      { gramKW $ PartOfSpeech $ Verb [Reflexive] }
  "adj"                     { gramKW $ PartOfSpeech Adjective }
  "adv"                     { gramKW $ PartOfSpeech Adverb }
  "prp"                     { gramKW $ PartOfSpeech Preposition }
  "conj"                    { gramKW $ PartOfSpeech Conjunction }
  "art"                     { gramKW $ PartOfSpeech Article }
  "pron"                    { gramKW $ PartOfSpeech $ Pronoun [] }
  "ppron"                   { gramKW $ PartOfSpeech $ Pronoun [Personal] }
  "pron interrog" / $word_end   { gramKW $ PartOfSpeech $
                                    Pronoun [Interrogative] }
  "pron relativ"  / $word_end   { gramKW $ PartOfSpeech $
                                    Pronoun [Relative] }
  "num"                     { gramKW $ PartOfSpeech Numeral }
  "interj"                  { gramKW $ PartOfSpeech Interjection }
  --
  "Gen."                    { gramKW $ Case Genitive }
  "Akk."                    { gramKW $ Case Accusative }
  "Dat."                    { gramKW $ Case Dative }

  -- Interrogative pronouns (not grammar keywords in the strict sense)
  "wo?"                     { regularToken IntPronKW }
  "wohin?"                  { regularToken IntPronKW }
  "wann?"                   { regularToken IntPronKW }
  "bis wann?" / $word_end   { regularToken IntPronKW }

  -- Regular word / text token
  -- Note:
  --  * This must come after all the specific keywords, or else this rule
  --    would be prefered over them (the single word keywords, to be precise).
  @word                                 { regularToken Text }

  -- Whitespace
  -- Note:
  --  * Newlines do not count as whitespace, they serve as line separators,
  --    where a line is a well defined Ding entity.
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

gramKW :: GramLexCategory -> AlexPosn -> String -> SimpleToken
gramKW gram p _ = RegularToken (toPosition p) (GramKW gram)

toPosition :: AlexPosn -> Position
toPosition (AlexPn _abs line col) = Position line col


-- | Remove the whitespace from the token stream towards the respective
--   following regular token, as annotation.
mergeWS :: [SimpleToken] -> [Token]

-- Notes
--  * There can never be two whitespace tokens in succession.
--  * One could use `foldr' here, but this is not very straightforward - the
--    folding function (in some cases) has to inspect the first element of its
--    list argument, which therefore is inspected twice (in such cases).

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
