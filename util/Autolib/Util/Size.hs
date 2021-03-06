{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts -fallow-undecidable-instances #-}

--  $Id$

module Autolib.Util.Size where

import Autolib.Set
import Autolib.FiniteMap

class Size a where 
    size :: a -> Int

-- instance Size a where
--     size = const 1 -- default

instance Size (Set a) where 
    size = cardinality

instance Size (FiniteMap a b) where
    size = sizeFM 

instance Size a => Size [a] where
    size = sum . map size

instance Size () where 
    size () = 1

instance ( Size a, Size b ) => Size (a, b) where
    size (x, y) = size x + size y

-- | split after accumulated size reaches limit
split :: Size a 
	=> Int 
	-> [a]
	-> ( [a], [a] )
split lim xs | lim < 0 || null xs = ( [], xs )
split lim (x : xs) = 
    let ( ys, zs ) = split ( lim - size x ) xs
    in	( x : ys, zs )

instance Size Char where size = const 1
instance Size Int  where size = const 1


