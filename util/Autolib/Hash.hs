module Autolib.Hash where

--  $Id$

import Data.Int
import Data.HashTable (hashInt)

import Data.FiniteMap
import Data.Set

class Eq a => Hash a where
      hash :: a -> Int32

instance Hash () where
    hash () = 1855

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

instance Hash Int where hash = hashInt . fromIntegral
instance Hash Int16 where hash = hashInt . fromIntegral
instance Hash Int32 where hash = id

instance Hash Char where 
    hash = fromIntegral . fromEnum

instance ( Hash a, Hash b ) => Hash ( Either a b ) where
    hash ( Left a ) = hash ( 13 :: Int, a )
    hash ( Right b ) = hash ( 31 :: Int, b )

instance Hash Bool where hash = fromIntegral . fromEnum

instance Hash a => Hash ( Maybe a ) where
    hash ( Just x ) = hash ( 41 :: Int, x )
    hash Nothing    = 14

