module Autolib.Reader.Basic 

( parse_complete
, readerParen
, my_parens, my_braces, my_brackets
, my_comma, my_semi, my_dot, my_star
, my_reserved, my_equals
, my_commaSep, my_semiSep
, my_identifier, my_symbol
, my_integer
, my_stringLiteral
, my_whiteSpace
, parsed_info
)

where

--   $Id$

import Autolib.Reader.Class
-- import Autolib.TypeOf -- TODO: what for? Idee ausbauen!

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language ( haskell )

import qualified Text.PrettyPrint.HughesPJ as Pretty

import Control.Monad ( guard )

parse_complete :: Parser p -> Parser p
parse_complete p = do 
    whiteSpace haskell
    x <- p
    eof
    return x

my_parens :: Parser a -> Parser a
my_parens = parens haskell
my_brackets :: Parser a -> Parser a
my_brackets = brackets haskell
my_braces :: Parser a -> Parser a
my_braces = braces haskell

my_comma :: Parser String
my_comma = comma haskell
my_semi :: Parser String
my_semi = semi haskell
my_dot :: Parser String
my_dot = dot haskell
my_star :: Parser ()
my_star = reservedOp haskell "*"
my_equals :: Parser ()
my_equals = reservedOp haskell "="

my_reserved :: String -> Parser ()
my_reserved = reserved haskell
my_commaSep :: Parser a -> Parser [a]
my_commaSep = commaSep haskell
my_semiSep :: Parser a -> Parser [a]
my_semiSep = semiSep haskell
my_identifier :: Parser String
my_identifier = identifier haskell
my_symbol :: String ->  Parser String
my_symbol = symbol haskell

my_stringLiteral :: Parser String
my_stringLiteral = stringLiteral haskell
my_integer :: Parser Integer
my_integer = integer haskell
my_whiteSpace :: Parser ()
my_whiteSpace = whiteSpace haskell

readerParen :: Bool -> Parser a -> Parser a
readerParen man p =
    my_parens p <|> do	guard $ not man ; p

-- | das schreiben wir in alle *_info - komponenten
-- die werden nicht geparst
parsed_info :: Parser Pretty.Doc
parsed_info = return $ Pretty.doubleQuotes $ Pretty.text "parsed_info"

--------------------------------------------------------------------------
