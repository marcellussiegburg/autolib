{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts #-}
{-# language IncoherentInstances #-}

module Autolib.Reader.Instances where

--   $Id$

import Autolib.Reader.Class
import Autolib.Reader.Basic
import Autolib.ToDoc (Doc, text)

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
-- import Text.ParserCombinators.Parsec.Language ( haskell )

import Data.Int
import Data.Ratio

instance Reader Integer where atomic_reader = integer haskell
instance Reader Int   where atomic_reader = fmap fromIntegral $ integer haskell
instance Reader Int32 where atomic_reader = fmap fromIntegral $ integer haskell
instance Reader Int16 where atomic_reader = fmap fromIntegral $ integer haskell
instance Reader Char    where atomic_reader = charLiteral haskell
instance Reader String  where atomic_reader = stringLiteral haskell
instance Reader Double where atomic_reader = float haskell

instance Reader Rational where
    atomic_reader = try ( do my_whiteSpace
			     x <- reader
                             my_whiteSpace
			     reservedOp haskell "%" <|> reservedOp haskell "/"
                             my_whiteSpace
			     y <- reader ; my_whiteSpace
			     return $ x % y
			)
		    <|> ( do my_whiteSpace 
                             x <- reader ; my_whiteSpace
			     return $ x % 1
			)


instance Reader () where 
    atomic_reader = my_parens ( return () )


instance (Reader a, Reader b) => Reader (a, b) where
    atomic_reader = my_parens $ do 
	     x <- reader ; my_comma
	     y <- reader
	     return (x, y)

instance (Reader a, Reader b, Reader c) => Reader (a, b, c) where
    atomic_reader = my_parens $ do 
	     x <- reader ; my_comma
	     y <- reader ; my_comma
	     z <- reader
	     return (x, y, z)


instance (Reader a, Reader b, Reader c, Reader d ) => Reader (a, b, c, d) where
    atomic_reader = my_parens $ do 
	     x <- reader ; my_comma
	     y <- reader ; my_comma
	     z <- reader ; my_comma
	     p <- reader 
	     return (x, y, z, p)


instance (Reader a, Reader b, Reader c, Reader d, Reader e ) => Reader (a, b, c, d, e) where
    atomic_reader = my_parens $ do 
	     x <- reader ; my_comma
	     y <- reader ; my_comma
	     z <- reader ; my_comma
	     p <- reader ; my_comma
	     q <- reader
	     return (x, y, z, p, q)




instance Reader a => Reader [a] where
    atomic_reader = listify reader

listify :: Parser a -> Parser [a] 
listify p = my_brackets ( commaSep haskell p )


instance Reader Doc where
    reader = do cs <- reader ; return $ text cs
