-- $Header$

module Size where

import Set
import FiniteMap

class Size a where size :: a -> Int

instance Size (Set a) where 
    size = cardinality

instance Size b => Size (FiniteMap a b) where
    size = sum . map size . eltsFM 