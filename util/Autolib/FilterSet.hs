module Autolib.FilterSet where

import Autolib.Set

filterSet ::  Ord a => (a -> Bool) -> Set a -> Set a
filterSet p = mkSet . filter p . setToList


