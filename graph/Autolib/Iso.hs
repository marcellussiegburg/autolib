--   $Id$

module Iso where

import Data.Set

-- | Klasse fuer den Isomorphietest
class Iso a where
  iso :: a -> a -> Bool


-- Instanzen für die Standart-Datentypen: Int, Integer, Char, () , []
 
instance Iso Int where
  iso a b = a == b

instance Iso Integer where
  iso a b = a == b

instance Iso Char where
  iso a b = a == b

instance Iso a => Iso [a] where
  iso a b = and $ map (\(x,y) -> iso x y) (zip a b)

instance (Ord a, Iso a) => Iso (Set a) where
  iso a b = iso (setToList a) (setToList b)

instance (Iso a, Iso b) => Iso (a,b) where
  iso (a,b) (c,d) = and [iso a c, iso b d]




