module Relation 

( Type, Make
, images
, make
, pairs
, times, plus, trans
)

where

-- $Id$

import Sets
import FiniteMap
import Fix
import ToDoc

data Type a b = Make { unRelation :: FiniteMap a (Set b) }

instance ( Ord a, Ord b, ToDoc a, ToDoc b ) => ToDoc ( Type a b ) where
    toDoc r = text "Relation.make" <+> toDoc ( pairs r )

instance ( Ord a, Ord b, ToDoc a, ToDoc b ) => Show ( Type a b ) where
    show = render . toDoc

instance ( Ord a, Ord b ) => Eq ( Type a b ) where
    r == s = unRelation r == unRelation s

make :: (Ord a, Ord b) => [(a,b)] -> Type a b
make xys = Make $ addListToFM_C union emptyFM $ do
	   (x, y) <- xys 
	   return (x, unitSet y)

pairs :: (Ord a, Ord b) => Type a b -> [(a,b)] 
pairs rel = do
    (x, ys) <- fmToList $ unRelation rel
    y <- setToList ys
    return (x, y)

lookupset fm = lookupWithDefaultFM fm emptySet

images :: ( Ord a, Ord b ) => Type a b -> a -> Set b
images rel x = lookupset (unRelation rel) x

times :: (Ord a, Ord b, Ord c)
      =>  Type a b -> Type b c -> Type a c
times r s = make $  do 
    (x, y) <- pairs r
    z <- setToList $ images s y
    return (x, z)

plus :: (Ord a, Ord b)
     => Type a b -> Type a b -> Type a b
plus r s = Make
	 $ plusFM_C union (unRelation r) (unRelation s)


trans :: Ord a => Type a a -> Type a a
-- transitive hülle (nicht reflexive)
trans r = fix ( \ s -> plus r $ times r s ) r
