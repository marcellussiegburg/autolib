module Reader.Basic 

( parsec_readsPrec
, readerParen
, my_parens, my_braces, my_brackets
, my_comma, my_semi, my_dot
, my_reserved, my_equals
, my_commaSep, my_semiSep
, my_identifier
, my_integer
, my_stringLiteral
, parsed_info
)

where

--   $Id$

import Reader.Class
import TypeOf -- TODO: what for? Idee ausbauen!

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language ( haskell )

import qualified Text.PrettyPrint.HughesPJ as Pretty

import Control.Monad ( guard )

parsec_readsPrec :: Reader a => Int -> ReadS a
parsec_readsPrec p input = 
    case parse ( do whiteSpace haskell
	            x <- readerPrec p
	            rest <- getInput
	            return (x,rest) ) 
	        "input" input 
    of Right (x, rest) -> return (x, rest)
       Left  err       -> error ("\n" ++ input ++ "\n" ++ show err)

my_parens = parens haskell
my_brackets = brackets haskell
my_braces = braces haskell
my_comma = comma haskell
my_semi = semi haskell
my_dot = dot haskell
my_equals = reservedOp haskell "="
my_reserved = reserved haskell
my_commaSep = commaSep haskell
my_semiSep = semiSep haskell
my_identifier = identifier haskell
my_stringLiteral = stringLiteral haskell
my_integer = integer haskell


readerParen man p =
    my_parens p <|> do	guard $ not man ; p

-- das schreiben wir in alle *_info - komponenten
-- die werden nicht geparst
parsed_info :: Parser Pretty.Doc
parsed_info = return $ Pretty.doubleQuotes $ Pretty.text "parsed_info"

--------------------------------------------------------------------------
