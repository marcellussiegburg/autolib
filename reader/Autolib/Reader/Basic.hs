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

my_parens = parens haskell
my_brackets = brackets haskell
my_braces = braces haskell
my_comma = comma haskell
my_semi = semi haskell
my_dot = dot haskell
my_star = reservedOp haskell "*"
my_equals = reservedOp haskell "="
my_reserved = reserved haskell
my_commaSep = commaSep haskell
my_semiSep = semiSep haskell
my_identifier = identifier haskell
my_symbol = symbol haskell
my_stringLiteral = stringLiteral haskell
my_integer = integer haskell
my_whiteSpace = whiteSpace haskell

readerParen man p =
    my_parens p <|> do	guard $ not man ; p

-- das schreiben wir in alle *_info - komponenten
-- die werden nicht geparst
parsed_info :: Parser Pretty.Doc
parsed_info = return $ Pretty.doubleQuotes $ Pretty.text "parsed_info"

--------------------------------------------------------------------------
