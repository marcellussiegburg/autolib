{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Autolib.Letters where

--   $Id$

import Autolib.Set

class Ord b => Letters a b | a -> b where
      letters :: a -> Set b

instance ( Letters a c, Letters b c ) => Letters (a, b) c where
    letters (a, b) = union (letters a) (letters b)

instance Letters a c => Letters [a] c where
    letters xs = unionManySets $ map letters xs

