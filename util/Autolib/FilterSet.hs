module Autolib.FilterSet where

import Data.Set

filterSet ::  Ord a => (a -> Bool) -> Set a -> Set a
filterSet p = mkSet . filter p . setToList


