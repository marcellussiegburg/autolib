-- $Header$

module Relation where

-- $Log$
-- Revision 1.1  2002-05-24 10:46:48  challenger
-- Initial revision
--
-- Revision 1.1  2001/11/18 23:55:23  autotool
-- neu: relation, fixpunkt
-- todoc: instance Either
--

import Set
import FiniteMap
import Fix

type Relation a b = FiniteMap a (Set b)

lookupset fm = lookupWithDefaultFM fm emptySet

times :: (Ord a, Ord b, Ord c)
      =>  Relation a b -> Relation b c -> Relation a c
times r s = listToFM $  do 
    (x, ys) <- fmToList r
    return ( x, mkSet $ do
	       y <- setToList ys
	       setToList $ lookupset s y
	   )

plus :: (Ord a, Ord b)
     => Relation a b -> Relation a b -> Relation a b
plus r s = addListToFM_C union r $ fmToList s


trans :: Ord a => Relation a a -> Relation a a
trans r = fix ( \ s -> plus r $ times r s ) r
