--  $Id$

module Util.Size where

import Data.Set
import Data.FiniteMap

class Size a where size :: a -> Int

instance Size (Set a) where 
    size = cardinality

instance Size (FiniteMap a b) where
    size = length . eltsFM 

instance Size a => Size [a] where
    size = sum . map size

instance Size Char where size = const 1
instance Size Int  where size = const 1
