{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}

module Autolib.Reader.Instances where

--   $Id$

import Autolib.Reader.Class
import Autolib.Reader.Basic

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language ( haskell )

import Data.Int

instance Reader Integer where reader = integer haskell
instance Reader Int   where reader = fmap fromIntegral $ integer haskell
instance Reader Int32 where reader = fmap fromIntegral $ integer haskell
instance Reader Int16 where reader = fmap fromIntegral $ integer haskell
instance Reader Char    where reader = charLiteral haskell
instance Reader String  where reader = stringLiteral haskell
instance Reader Double where reader = float haskell


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


instance (Reader a, Reader b, Reader c, Reader d ) => Reader (a, b, c, d) where
    reader = my_parens $ do 
	     x <- reader ; my_comma
	     y <- reader ; my_comma
	     z <- reader ; my_comma
	     p <- reader 
	     return (x, y, z, p)


instance (Reader a, Reader b, Reader c, Reader d, Reader e ) => Reader (a, b, c, d, e) where
    reader = my_parens $ do 
	     x <- reader ; my_comma
	     y <- reader ; my_comma
	     z <- reader ; my_comma
	     p <- reader ; my_comma
	     q <- reader
	     return (x, y, z, p, q)




instance Reader a => Reader [a] where
    reader = listify reader

listify :: Parser a -> Parser [a] 
listify p = my_brackets ( commaSep haskell p )


