module Hash where

--  $Id$

import GHC.Int

import Data.FiniteMap
import Data.Set

class Eq a => Hash a where
      hash :: a -> Int	-- TODO: should be unboxed word or something

instance (Hash a, Hash b) => Hash (a, b) where
    -- not recommended (should be cached instead)
    hash (a, b) = hash a + 3 + 107 * hash b

instance (Hash a, Hash b, Hash c) => Hash (a, b, c) where
    -- not recommended (should be cached instead)
    hash (a, b, c) = hash (a, hash (b, c))

instance (Hash a, Hash b, Hash c, Hash d) => Hash (a, b, c, d) where
    -- not recommended (should be cached instead)
    hash (a, b, c, d) = hash ((a, b), hash (c, d))


instance (Hash a) => Hash [a] where
    hash [] = 314159
    hash (x : xs) = hash (x, xs) 

instance Hash a => Hash ( Set a ) where
    hash = hash . setToList

instance ( Hash a, Hash b ) => Hash ( FiniteMap a b ) where
    hash = hash . fmToList

instance Hash Int where hash = fromIntegral

instance Hash Int16 where hash = fromIntegral

instance Hash Char where 
    hash = fromEnum

instance ( Hash a, Hash b ) => Hash ( Either a b ) where
    hash ( Left a ) = hash ( 13 :: Int, a )
    hash ( Right b ) = hash ( 31 :: Int, b )

instance Hash Bool where hash = fromEnum

    