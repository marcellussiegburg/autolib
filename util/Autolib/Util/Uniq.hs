module Util.Uniq where

-- $Id$

import Sets

-- produce a lazy (!) sublist of entries with all duplicates removed

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : filter ( /= x ) ( uniq xs )

uniqs :: Ord a => [a] -> [a]
uniqs xs = helper emptySet xs where
    helper done [] = []
    helper done ( x : xs ) = 
	   if elementOf x done
	   then helper done xs
	   else x : helper ( addToSet done x ) xs

