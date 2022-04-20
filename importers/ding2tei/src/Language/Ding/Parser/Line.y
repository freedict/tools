{-
 - Language/Ding/Parser/Line.y - parser for a single Ding line
 -
 - Copyright 2020-2022 Einhard Leichtfuß
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


-- Notes:
--  * This file is supposed to be processed by Happy.  It contains both Happy
--    specific syntax and regular Haskell code.  The former is in some ways
--    similar to the latter.  The result of processing with Happy is a regular
--    Haskell source file.
--  * The Happy syntax should be very similar to the syntax of Yacc/Bison.
--  * There is an important relation between the lexer (produced with the help
--    of Alex) and the parser.
--  * Happy Documentation: https://www.haskell.org/happy/


{
-- Haskell module header and import statements.

{-# LANGUAGE FlexibleInstances #-}

{-|
 - Parse single Ding lines from a list of tokens.  Only accepts newline
 - terminated lines.
 -}
module Language.Ding.Parser.Line (parseLine, FailWriter) where

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(fail))
import Control.Monad.Writer
  (Writer, tell, writer, WriterT(WriterT), mapWriterT)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NEList
import Safe.Exact (zipWithExactMay)

import Data.NatLang.Grammar
  ( GrammarInfo(..)
  , GramLexCategory(..)
  , Case(..)
  , Collocate(..)
  )
import Data.NatLang.Usage (Usage)
import Data.NatLang.InflectedForms (InflectedForms(..), InflectedForm(..))
import Language.Ding.Partial.Unit (PartialUnit)
import qualified Language.Ding.Partial.Unit as PU
import Language.Ding.Partial.PseudoUnit (PartialPseudoUnit)
import qualified Language.Ding.Partial.PseudoUnit as PSU
import Language.Ding.Read.Usage (readUsage)
import Language.Ding.Syntax
import Language.Ding.Token
  (Token(..), Atom(..), Position(..), tokenToString, tokenToLine)
}


%name parseLine         -- name of the resulting function
%tokentype { Token }
%monad { FailWriter }
%error { parseError }   -- name of the error function - must be defined later


-------------------------------------------------------------------------------
-- Link Happy tokens (left) to Haskell patterns (right).

-- Notes:
--  * If `$$' is specified on the right, the Happy token will evaluate to the
--    token's part matched by `$$'.  Otherwise, the Happy token evaluates to
--    the whole token.
--  * It is common for simple tokens to use as name the string representing the
--    token, however not at all necessary.

%token '\n'             { Token _ _ NL }
       '::'             { Token _ _ LangSep }
       '|'              { Token _ _ Vert }
       ';'              { Token _ _ Semi }
       ','              { Token _ _ Comma }
       '~'              { Token _ _ Tilde }
       '+'              { Token _ _ Plus }
       '<>'             { Token _ _ Wordswitch }
       '/'              { Token _ _ StrongSlash }
       '</>'            { Token _ _ WeakSlash }
       '//'             { Token _ _ DoubleSlash }

       '{'              { Token _ _ OBrace }
       '}'              { Token _ _ CBrace }
       '['              { Token _ _ OBracket }
       ']'              { Token _ _ CBracket }
       '('              { Token _ _ OParen }
       ')'              { Token _ _ CParen }
       '<'              { Token _ _ OAngle }
       '>'              { Token _ _ CAngle }
       '</'             { Token _ _ OSlash }
       '/>'             { Token _ _ CSlash }

       'to'             { Token _ _ KW_to }

       tok_slashSpecial { Token _ _ (SlashSpecial _) }
       tok_abbrev       { Token _ _ (Abbrev _) }
       tok_abbrevPlural { Token _ _ (AbbrevPlural _) }

       -- Note:
       --  * The grammar annotations are separated because gramCase needs to be
       --    separately available (and Happy tokens may not overlap).
       tok_gramPOS      { Token _ _ (GramKW (PartOfSpeech _)) }
       tok_gramGender   { Token _ _ (GramKW (Gender _)) }
       tok_gramNumber   { Token _ _ (GramKW (Number _)) }
       tok_gramCase     { Token _ _ (GramKW (Case _)) }

       tok_interrogPron { Token _ _ (IntPronKW _) }
       tok_text         { Token _ _ (Text _) }



-- Just ensure that 'to' has lowest precedence.
--  * Giving it right associativity causes 'to to' to cause a shift.
%right 'to'
%right ',' '~' '+' '<>' '/' '</>' '//' tok_gramPOS tok_gramGender
  tok_gramNumber tok_gramCase tok_interrogPron tok_text
  '('

%%


-------------------------------------------------------------------------------
-- Grammar specification
-------------------------------------------------------------------------------

-- See doc/ding-grammar-naming for a brief syntax overview and naming
-- conventions.

-- Notes:
--  * Only a single line is parsed.
--  * Left recursion is recommended by the Happy developers, due to space
--    requirements.
--    * This causes lists to be reversed, which requires re-reversal (or
--      special alternative list structures), which makes the grammar often
--      less readable.
--    * Since all lists are expected to be small, space should not be an issue.
--      * Hence, ignore this.
--    - https://www.haskell.org/happy/doc/html/sec-sequences.html
--    - String-concatenation that uses left-recursion has the same problem.
--      - (<>), when applied to tokens does essentially perform string
--        concatentation.
--        - mconcat can be used instead.
--  * The parsing happens in the Writer monad to allow for logging.
--    * There does however not happen any logging as of now.
--    * The syntax would be the same without a monad; only the type of the
--      parseLine function would be simpler.


-- Note:
--  * The newline token serves as EOL token, such that there is never an error
--    on end of tokens.
--    * (One could also have given this parser a non-newline terminated stream
--      of tokens.)
--    * End-of-tokens errors do not report the position of the error, which
--      usually is not a problem, since it is just the end of input.
--      * However, since we only parse single lines, where each contained
--        token is annotated with a line number, this would be a problem.
line :: { Line }
line : groups '::' groups '\n'          {% makeLine (tokenToLine $2) $1 $3 }

groups :: { [Group] }
groups : group '|' groups               { $1 : $3 }
       | group                          { $1 : [] }

-- Note:
--  * In some cases, there is exactly one unit in a group, which does not obey
--    to the rules of a proper unit - the headword is enclosed in <()>.
--    * It does however (in practice) obey the rules of a PartialPseudoUnit.
--      * Use these, to ignore the unit.  (TODO: improve)
--    * See also: todo/parsing.paren-units
group :: { Group }
group : units                           { Group $ reverse $1 }
      | {- epsilon -}                   { Group [] }
      | pseudoUnit                      { Group [] }

-- Notes:
--  * Produces the list of units in reverse order.
--    * This is to ease the application of `addToUnit'.
--  * Only accepts a non-empty list of units.  (because of the <;> seps)
units :: { [Unit] }
units : units ';' unit                  { $3 : $1 }
      | units ';' pseudoUnit            { map (PSU.addToUnit $3) $1 }
      | unit                            { $1 : [] }


-------------------------------------------------------------------------------
-- Unit

unit :: { Unit }
unit : parenExps partialUnit            { PU.toUnit $1 $2 }

-- TODO: Do not ignore <>.
pseudoUnit :: { PartialPseudoUnit }
pseudoUnit : pseudoUnit gramAnnot       { $1 `PSU.plusGramAnnot` $2 }
           | pseudoUnit usageAnnot      { $1 `PSU.plusUsageAnnot` $2 }
           | pseudoUnit angleExp        { $1 }
           | parenExp parenExps         { PSU.fromSuffixes ($1 : $2) }

-- Notes:
--  * Any infix annotations are silently dropped (see plusToken).  Prefix
--    annotations are considered invalid and not accepted by the grammar.
--    * Parenthesis expressions may occur as prefixes though, they are
--      recognized in the `unit' rule.
--  * Initial abbreviations are equated to regular text, iff they are "single",
--    that is contain no <,> or <;> separated list of abbeviations.
--  * If prefixed by 'to', ignore the 'to' and mark as verb.
--    * This only happens if 'to' is immediately followed by something
--      recognized as regular text (unitText).
--      * In particular things like 'to (have a) lisp' or
--        'to (give one's) consent (to)' are not specially treated.
--        (They are quite infrequent though.)
--    * Also, allowing () would complicate the parsing.
--    * PU.fromVerbToken annotates the unit as a verb.  This may be superfluous
--      if there is an explicit annotation (rare), but such a duplicate would
--      be resolved during enrichment.
--  * angleExp ("<...>") is ignored.  (TODO?)
--    * Semantics unclear.
partialUnit :: { PartialUnit }
partialUnit : partialUnit gramAnnot     { $1 `PU.plusGramAnnot` $2 }
            | partialUnit usageAnnot    { $1 `PU.plusUsageAnnot` $2 }
            | partialUnit abbrevAnnot   { $1 `PU.plusAbbrevAnnot` $2 }
            | partialUnit inflAnnot     { $1 `PU.plusInflAnnot` $2 }
            | partialUnit parenExp      { $1 `PU.plusSuffix` $2 }
            | partialUnit angleExp      { $1 {- ignore $2 -} }
            | partialUnit reference     { $1 `PU.plusRef` $2 }
            | partialUnit unitText      { $1 `PU.plusToken` $2 }
            | partialUnit 'to'          { $1 `PU.plusToken` $2 }
            | partialUnit unitInterpct  { $1 `PU.plusToken` $2 }
            | unitText                  { PU.fromToken $1 }

            -- 'to' has lowest precedence (and is right associative), so the
            -- later rules will be preferred.
            | 'to'                      { PU.fromToken $1 }
            | 'to' unitInterpct         { PU.fromToken ($1 <> $2) }
            | 'to' unitText             { PU.fromVerbToken $2 }

            -- Some units start with an abbreviation (usually without further
            -- unitText).  In such, treat the abbreviation as plain text.
            | singleAbbrevAnnot         { PU.fromToken $1 }

-- Notes:
--  * As of now, keywords (except for 'to') have no special meaning in
--    top-level text.  Hence, treat them as simple text.
--  * Cannot use gtok_anyKW, since it includes 'to'.
unitText :: { Token }
unitText : tok_text                 { $1 }
         | gtok_gram                { $1 }
         | tok_interrogPron         { $1 }

-- | Interpunctuation in units.
-- 
--   Note: "<>" is ignored.
unitInterpct :: { Token }
unitInterpct : ','                      { $1 }
             | '+'                      { $1 }
             | '//'                     { $1 }
             | '/'                      { $1 }
             | '</>'                    { $1 }
             | '<>'                     { mempty }


-------------------------------------------------------------------------------
-- Grammar annotation

gramAnnot :: { NonEmpty GrammarInfo }
gramAnnot : '{' gramAnnot1s '}'           { $2 }

-- Note: <;> and <,> are used with different meanings in this context
--       (usually AND and OR, respectively), but to keep things simple, they
--       are considered equivalent as of now.
gramAnnot1s :: { NonEmpty GrammarInfo }
gramAnnot1s : gramAnnot1 ';' gramAnnot1s  { $1 <> $3 }
            | gramAnnot1                  { $1 }

gramAnnot1 :: { NonEmpty GrammarInfo }
gramAnnot1 : gramAnnot2s                  { $1 }
           | collocAnnot1 usageAnnots     { Collocate $1 $2 :| [] }

-- The two rules for CollocCase are necessary to avoid a shift/reduce conflict.
-- The first rule only allows a non-empty list of interrogation pronouns.
collocAnnot1 :: { Collocate }
collocAnnot1 : interrogProns
               '+' tok_gramCase           { CollocCase $1 (tokenToCase $3)  }
             | '+' tok_gramCase           { CollocCase [] (tokenToCase $2)  }
             | '+' tok_gramPOS            { CollocPOS (tokenToPOS $2)       }
             | '+' tok_gramNumber         { CollocNumber (tokenToNumber $2) }

-- TODO: Consider to add '/' as alternative separator.
gramAnnot2s :: { NonEmpty GrammarInfo }
gramAnnot2s : gramAnnot2 ',' gramAnnot2s  { $1 <| $3 }
            | gramAnnot2                  { $1 :| [] }

gramAnnot2 :: { GrammarInfo }
gramAnnot2 : gramLexCat                   { GramLexCategory $1 }

gramLexCat :: { GramLexCategory }
gramLexCat : gtok_gram                    { tokenToGramLexCat $1 }

interrogProns :: { [String] }
interrogProns : tok_interrogPron ',' interrogProns  { tokenToString $1 : $3 }
              | tok_interrogPron                    { tokenToString $1 : [] }


-------------------------------------------------------------------------------
-- Inflected forms

-- Notes:
--  * Inflected forms are distinguished from grammar annotations by the
--    absence of grammar keywords.
--  * NonEmpty lists are used here.  (:|) constructs a NonEmpty list from a
--    single head element and a regular list.  (<|) joins a single element with
--    a NonEmpty list, just like (:) for normal lists.
--    * Considering the implementation of NonEmpty, using (<|) is not very
--      efficient (decons (:|), cons (:), cons (:|) - instead of just
--      cons (:)).
inflAnnot :: { InflectedForms }
inflAnnot : '{' inflAnnot1s ';' inflAnnot1s '}'   { InflectedForms $2 $4 }

inflAnnot1s :: { NonEmpty InflectedForm }
inflAnnot1s : inflAnnot1 ',' inflAnnot1s  { $1 <| $3 }
            | inflAnnot1                  { $1 :| [] }

inflAnnot1 :: { InflectedForm }
inflAnnot1 : inflAnnot2 usageAnnots     { InflectedForm (tokenToString $1) $2 }

-- Notes:
--  * Most inflected forms consist of a single word.  Not all
--    (e.g., "creeped out").
--  * Cannot use gtok_anyKW, as it includes gtok_gramKW.
inflAnnot2 :: { Token }
inflAnnot2 : tok_text inflAnnot2        { $1 <> $2 }
           | tok_text                   { $1 }
           | tok_interrogPron           { $1 }
           | 'to'                       { $1 }


-------------------------------------------------------------------------------
-- Abbreviation annotations

-- Note: This rule exists purely to provide a prefix for unit.
singleAbbrevAnnot :: { Token }
singleAbbrevAnnot : '</' abbrevAnnot1 '/>'  { $2 }
                  | tok_abbrev              { $1 }
                  | tok_abbrevPlural        { $1 }
                  | tok_slashSpecial        { $1 }

abbrevAnnot :: { NonEmpty String }
abbrevAnnot : '</' abbrevAnnot1s '/>'   { $2 }
            | tok_abbrev                { tokenToString $1 :| [] }
            | tok_abbrevPlural          { (tokenToString $1 ++ "s") :| [] }
            | tok_slashSpecial          { tokenToString $1 :| [] }

-- Note: <;> is more frequent.
abbrevAnnot1s :: { NonEmpty String }
abbrevAnnot1s : abbrevAnnot1 ';' abbrevAnnot1s  { tokenToString $1 <| $3 }
              | abbrevAnnot1 ',' abbrevAnnot1s  { tokenToString $1 <| $3 }
              | abbrevAnnot1                    { tokenToString $1 :| [] }

-- Consider content a literal (in particular do not drop any inner annotation).
abbrevAnnot1 :: { Token }
abbrevAnnot1 : abbrevAnnot2 abbrevAnnot1  { $1 <> $2 }
             | abbrevAnnot2               { $1 }

-- Note:
--  * <()>, <[]> are rare here.
--    . Ex.: "/Hg(CNO)2/"
--    . Ex.: "section 15, subsection 3 /s.15[3]/"
--    * Nesting likely does not occur at all, but is considered valid.
abbrevAnnot2 :: { Token }
abbrevAnnot2 : tok_text                 { $1 }
             | gtok_anyKW               { $1 }
             | '+'                      { $1 }
             | '/'                      { $1 }
             | '(' abbrevAnnot2 ')'     { $1 <> $2 <> $3 }
             | '[' abbrevAnnot2 ']'     { $1 <> $2 <> $3 }


-------------------------------------------------------------------------------
-- Expressions enclosed in parentheses

-- Note:
--  * The below rules give (String, Token) for a parenthesis expression.
--    The string is the content, the token joins the content with the
--    parentheses (and retains any spacing).
--    * The token is required to allow for infix parenthesis expressions to be
--      merged back with its context.

parenExps :: { [(String, Token)] }
parenExps : parenExp parenExps          { $1 : $2 }
          | {- epsilon -}               { [] }

parenExp :: { (String, Token) }
parenExp : '(' parenExp1 ')'            { (tokenToString $2, $1 <> $2 <> $3) }

parenExp1 :: { Token }
parenExp1 : parenExp2 parenExp1         { $1 <> $2 }
parenExp1 : parenExp2                   { $1 }

-- Notes:
--  * Several inner annotations are ignored (on purpose), such as references or
--    <{}>-annotations.
--    * Nonetheless, for example <{}>-annotations should not be simplified
--      (allowed to contain more or less anything) - the content should still
--      be a valid annotation.  I.e., use the grammar rules for those
--      annotations.
--  * Nested <()>-annotations are included literally.
--    * They are rare though.  (Hence the nested (<>) application is ok.)
--  * One might consider to include abbreviations literally.  (TODO?)
--    * This would require an abbrevAnnot'-rule, producing a token instead of
--      a list of Strings.
--      * Alternatively, one might reconstruct an equivalent representation,
--        based on that list.
parenExp2 :: { Token }
parenExp2 : tok_text                    { $1 }
          | gtok_anyKW                  { $1 }
          | reference                   { mempty }
          | gramAnnot                   { mempty }
          | inflAnnot                   { mempty }
          | usageAnnot                  { mempty }
          | abbrevAnnot                 { mempty }
          | '(' parenExp1 ')'           { $1 <> $2 <> $3 }
          | ';'                         { $1 }
          | ','                         { $1 }
          | '+'                         { $1 }
          | '<>'                        { mempty }
          | '/'                         { $1 }
          | '</>'                       { $1 }
          | '//'                        { $1 }


-------------------------------------------------------------------------------
-- Expressions enclosed in angle brackets

-- Note: They are dropped as of now, since their semantics are ambiguous.

angleExp :: { NonEmpty String }
angleExp : '<' angleExp1s '>'            { $2 }

-- Note:
--  * Separation by <;>, <,> is rare (two/one occurrences).
angleExp1s :: { NonEmpty String }
angleExp1s : angleExp1 ';' angleExp1s   { tokenToString $1 <| $3 }
           | angleExp1 ',' angleExp1s   { tokenToString $1 <| $3 }
           | angleExp1                  { tokenToString $1 :| [] }

angleExp1 :: { Token }
angleExp1 : angleExp2 angleExp1         { $1 <> $2 }
          | angleExp2                   { $1 }

-- Notes:
--  * There are nested angle-expressions.  It is unclear how to handle them.
--    Drop for now.  (TODO?)
--    * They are rare (two occurences).
--  * Only one occurence of nested <()>.  Dropped for now.  (TODO?)
--  * Only 4 occurrences of nested abbreviations.  Dropped for now.  (TODO?)
angleExp2 :: { Token }
angleExp2 : tok_text                    { $1 }
          | gtok_anyKW                  { $1 }
          | angleExp                    { mempty }
          | '(' angleExp1 ')'           { mempty }
          | abbrevAnnot                 { mempty }


-------------------------------------------------------------------------------
-- References ("~ref")

-- Note:
--  * Due to the simple current word-matching in the lexer, this might catch
--    unexpected references, such as "~word?". (TODO)
reference :: { String }
reference : '~' reference1              { tokenToString $2 }

reference1 :: { Token }
reference1 : tok_text                   { $1 }
           | gtok_anyKW                 { $1 }


-------------------------------------------------------------------------------
-- Usage annotations

-- Zero or more <[]>-enclosed usage annotations.
-- Unified to a single list (e.g., "[a,b] [c]" -> [a,b,c]).
usageAnnots :: { [Usage] }
usageAnnots : usageAnnot usageAnnots    { NEList.toList $1 ++ $2 }
            | {- epsilon -}             { [] }

-- A single usage annotation (e.g., "[a,b]").
usageAnnot :: { NonEmpty Usage }
usageAnnot : '[' usageAnnot1s ']'       { $2 }

-- Note: <,> and </> are treated as synonyms.  They possibly shouldn't.
usageAnnot1s :: { NonEmpty Usage }
usageAnnot1s : usageAnnot1 ',' usageAnnot1s   { $1 <| $3 }
             | usageAnnot1 '/' usageAnnot1s   { $1 <| $3 }
             | usageAnnot1                    { $1 :| [] }

usageAnnot1 :: { Usage }
usageAnnot1 : usageAnnot2s              { readUsage $ tokenToString $1 }

usageAnnot2s :: { Token }
usageAnnot2s : usageAnnot2 usageAnnot2s { $1 <> $2 }
             | usageAnnot2              { $1 }

usageAnnot2 :: { Token }
usageAnnot2 : tok_text                  { $1 }
            | gtok_anyKW                { $1 }
            | ';'                       { $1 }
            | '+'                       { $1 }


-------------------------------------------------------------------------------
-- Grouped simple tokens

gtok_anyKW :: { Token }
gtok_anyKW : gtok_gram                  { $1 }
           | tok_interrogPron           { $1 }
           | 'to'                       { $1 }

gtok_gram :: { Token }
gtok_gram : tok_gramPOS                 { $1 }
          | tok_gramGender              { $1 }
          | tok_gramNumber              { $1 }
          | tok_gramCase                { $1 }

{

-------------------------------------------------------------------------------
-- Auxiliary Haskell code


-- Logging:
--  * The FailWriter below allows for logging of parse notes and errors
--    (`fail').
--    - Errors indicate a failure and, so, no value is returned.
--  * `fail' should always be preferred over `error', unless the respective
--    condition indicates a programming error.
--  * The filename should not and cannot be given here; it is prepended at a
--    higher level.


-- | A writer that allows logging informative messages, but also to fail with
--   an error message.
--   The informative log is retained in case of a failure.
type FailWriter = WriterT [String] (Either String)

instance MonadFail (Either String) where
  fail = Left


-- Note: The token list can only be empty if the token list given to
--       `parseLine' was not terminated by a newline token.  Which should not
--       be possible (an error would occur on a higher level).
--  - The first error is thus deliberately handled by `error' and not `fail'.
parseError :: [Token] -> FailWriter a
parseError [] = error "Parse error at end of line (missing newline?)."
parseError ((Token _ (Position line col) _):_)
  = fail
  $ "Parse error at line " ++ show line ++ ", column " ++ show col ++ "."


-- | Create a line from the left and right side's groups.  Fail, if the groups
--   are not equal in number of entries.
makeLine :: Int -> [Group] -> [Group] -> FailWriter Line
makeLine line gs hs =
  case zipWithExactMay Entry gs hs of
    Just es -> return $ Line es
    Nothing -> fail
      $  "Line " ++ show line ++ ": "
      ++ "Number of groups does not match on the two sides."


tokenToGramLexCat :: Token -> GramLexCategory
tokenToGramLexCat (Token _ _ (GramKW gram)) = gram
tokenToGramLexCat _                         =
  error "Not a grammar keyword."

tokenToGramAnnot :: Token -> GrammarInfo
tokenToGramAnnot (Token _ _ (GramKW gram)) = GramLexCategory gram
tokenToGramAnnot _                         =
  error "Not a grammar annotation."

tokenToCase :: Token -> Case
tokenToCase (Token _ _ (GramKW (Case cas))) = cas
tokenToCase _                               =
  error "Not a case keyword."

tokenToPOS (Token _ _ (GramKW (PartOfSpeech pos))) = pos
tokenToPOS _                                       =
  error "Not a part of speech keyword."

tokenToNumber (Token _ _ (GramKW (Number number))) = number
tokenToNumber _                                    =
  error "Not a grammatical number keyword."

}


-- vi: ft=haskell ts=2 sw=2 et
