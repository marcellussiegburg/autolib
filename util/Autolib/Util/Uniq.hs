module Autolib.Util.Uniq where

-- -- $Id$

import Autolib.Sets

-- | produce a lazy (!) sublist of entries with all duplicates removed
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : filter ( /= x ) ( uniq xs )

-- | produce a lazy (!) sublist of entries with all duplicates removed
-- use a Set to cache the entries already seen
uniqs :: Ord a => [a] -> [a]
uniqs xs = helper emptySet xs where
    helper done [] = []
    helper done ( x : xs ) = 
	   if elementOf x done
	   then helper done xs
	   else x : helper ( addToSet done x ) xs

