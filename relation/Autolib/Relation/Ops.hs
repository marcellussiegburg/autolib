module Relation.Ops where

-- $Id$

import Relation.Type
import Fix

lookupset fm = lookupWithDefaultFM fm emptySet

holds :: ( Ord a, Ord b) => Type a b -> a -> b -> Bool
holds rel x y = y `elementOf` images rel x

images :: ( Ord a, Ord b ) => Type a b -> a -> Set b
images rel x = lookupset (unRelation rel) x

simages :: ( Ord a, Ord b ) => Type a b -> Set a -> Set b
simages rel xs = unionManySets $ do
    x <- setToList xs
    return $ images rel x

inverse :: ( Ord a, Ord b ) => Type a b -> Type b a
inverse r = ( make0 $ do ( x, y) <- pairs r ; return (y, x) )
	    { source = target r, target = source r }

identic :: Ord a => Set a -> Type a a
identic s = ( make0 $ do x <- setToList s ; return (x, x) )
	    { source = s , target = s }



times :: (Ord a, Ord b, Ord c)
      =>  Type a b -> Type b c -> Type a c
times r s = ( make0 $  do 
        (x, y) <- pairs r
        z <- setToList $ images s y
        return (x, z)
    )  { source = source r
	 , target = target s
       }

plus :: (Ord a, Ord b)
     => Type a b -> Type a b -> Type a b
plus r s = Make
	 { source = source r `union` source s
	 , target = target r `union` target s
	 , unRelation = plusFM_C union (unRelation r) (unRelation s)
	 }

trans :: Ord a => Type a a -> Type a a
-- transitive hülle (nicht reflexive)
trans r = fix ( \ s -> plus r $ times r s ) r

reflex :: Ord a => Type a a -> Type a a
reflex r = ( make0 $ do x <- setToList $ source r ; return (x, x) )
	 { source = source r
	 , target = source r -- ??
	 }

reflex_trans :: Ord a => Type a a -> Type a a
reflex_trans r = plus (reflex r) (trans r) 

minima :: Ord a => Type a a -> Set a
minima r = source r `minusSet` mkSet ( do (x,y) <- pairs r ; return y )

maxima :: Ord a => Type a a -> Set a
maxima r = target r `minusSet` mkSet ( do (x,y) <- pairs r ; return x )
