module Hash where

--  $Id$

import GHC.Int

class Eq a => Hash a where
      hash :: a -> Int	-- TODO: should be unboxed word or something

instance (Hash a, Hash b) => Hash (a, b) where
    -- not recommended (should be cached instead)
    hash (a, b) = hash a + 3 + 107 * hash b

instance (Hash a, Hash b, Hash c) => Hash (a, b, c) where
    -- not recommended (should be cached instead)
    hash (a, b, c) = hash (a, hash (b, c))


instance (Hash a) => Hash [a] where
    hash [] = 314159
    hash (x : xs) = hash (x, xs) 

instance Integral a => Hash a where
    {-# SPECIALIZE instance Hash Int #-}
    {-# SPECIALIZE instance Hash Int16 #-}
    hash = fromIntegral


instance Hash Char where 
    hash = fromEnum



    