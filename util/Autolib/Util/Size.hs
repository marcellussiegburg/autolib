-- $Id$

module Util.Size where

import Set
import FiniteMap

class Size a where size :: a -> Int

instance Size (Set a) where 
    size = cardinality

instance Size (FiniteMap a b) where
    size = length . eltsFM 

instance Size a => Size [a] where
    size = sum . map size
