module Reader.Basic 

( Reader (..)
, parse
, parsec_readsPrec
, readerParen
, (<|>)
, my_braces, my_comma
, my_reserved, my_equals
, listify
)

where

-- $Id$

import Reader.Class
import TypeOf 

import Parsec
import ParsecToken
import ParsecLanguage ( haskell )

import Monad ( guard )

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
my_equals = reservedOp haskell "="
my_reserved = reserved haskell


readerParen man p =
    my_parens p <|> do	guard $ not man ; p

--------------------------------------------------------------------------

instance Reader Integer where reader = integer haskell
instance Reader Char    where reader = charLiteral haskell
instance Reader String  where reader = stringLiteral haskell


instance Reader () where 
    reader = my_parens ( return () )


instance (Reader a, Reader b) => Reader (a, b) where
    reader = my_parens $ do 
	     x <- reader ; my_comma
	     y <- reader
	     return (x, y)

instance (Reader a, Reader b, Reader c) => Reader (a, b, c) where
    reader = my_parens $ do 
	     x <- reader ; my_comma
	     y <- reader ; my_comma
	     z <- reader
	     return (x, y, z)

instance Reader a => Reader [a] where
    reader = listify reader

listify :: Parser a -> Parser [a] 
listify p = my_brackets ( commaSep haskell p )

--------------------------------------------------------------------------
