module Autolib.Relation.Ops 

( module Autolib.Relation.Ops
, inverse
)

where

--   $Id$

import Autolib.Relation.Type

import Autolib.Set
import Autolib.Fix

import Autolib.Schichten ( bfs )

holds :: ( Ord a, Ord b) => Type a b -> a -> b -> Bool
holds rel x y = y `elementOf` images rel x

{-# INLINE images #-}

images :: ( Ord a, Ord b ) => Type a b -> a -> Set b
images rel x = lookupset (unRelation rel) x

trans_reflex_images ::  ( Ord a ) => Type a a -> a -> Set a
trans_reflex_images rel x = mkSet $ bfs ( \ x -> images rel x ) x

{-# INLINE pre_images #-}

pre_images :: ( Ord a, Ord b ) => Type a b -> b -> Set a
pre_images rel = images ( inverse rel )

{-# INLINE simages #-}

simages :: ( Ord a, Ord b ) => Type a b -> Set a -> Set b
simages rel xs = unionManySets $ do
    x <- setToList xs
    return $ images rel x

{-# INLINE pre_simages #-}

pre_simages :: ( Ord a, Ord b ) => Type a b -> Set b -> Set a
pre_simages rel = simages ( inverse rel )

identic :: Ord a => Set a -> Type a a
identic s = ( make0 $ do x <- setToList s ; return (x, x) )
	    { source = s , target = s }

rightmap :: ( Ord a, Ord b, Ord c)
	 => (b -> c) -> Type a b -> Type a c
rightmap f = bothmap id f

leftmap :: ( Ord a, Ord b, Ord c)
	 => (a -> b) -> Type a c -> Type b c
leftmap f = bothmap f id

bothmap :: ( Ord a, Ord b, Ord c, Ord d)
	 => (a -> b) -> (c -> d) 
	-> Type a c -> Type b d
bothmap f g r = 
    ( make0 $ do (x, y) <- pairs r ; return (f x, g y) )
    { source = smap f $ source r
    , target = smap g $ target r 
    }

{-# INLINE times #-}

times :: (Ord a, Ord b, Ord c)
      =>  Type a b -> Type b c -> Type a c
times r s = ( make0 $  do 
        (x, y) <- pairs r
        z <- setToList $ images s y
        return (x, z)
    )  { source = source r
	 , target = target s
       }

{-# INLINE plus #-}

plus :: (Ord a, Ord b)
     => Type a b -> Type a b -> Type a b
plus r s = Make
	 { source = source r `union` source s
	 , target = target r `union` target s
	 , unRelation = plusFM_C union (unRelation r) (unRelation s)
	 , inverse_unRelation = 
               plusFM_C union (inverse_unRelation r) (inverse_unRelation s)
	 }

difference :: (Ord a, Ord b)
     => Type a b -> Type a b -> Type a b
difference r s = R.make_on ( R.source r, R.target r ) $ do
    (x, y) <- R.pairs r
    guard $ not $ R.holds s x y
    return ( x, y )

intersection :: (Ord a, Ord b)
     => Type a b -> Type a b -> Type a b
intersection r s = R.make_on ( R.source r, R.target r ) $ do
    (x, y) <- R.pairs r
    guard $ R.holds s x y
    return ( x, y )

insert :: (Ord a, Ord b)
       => Type a b -> (a,b) -> Type a b
insert r (x,y) = r
       { unRelation = addToFM_C union (unRelation r) x ( mkSet [y] ) 
       , inverse_unRelation = 
           addToFM_C union (inverse_unRelation r) y ( mkSet [x] ) 
       }

inserts :: (Ord a, Ord b)
       => Type a b -> [(a,b)] -> Type a b
inserts r xys = plus r $ make xys

{-# INLINE trans #-}

-- | transitive closure (NB: not necessarily reflexive)
trans :: Ord a => Type a a -> Type a a
trans r = fix ( \ s -> plus r $ times r s ) r

-- |  reflexive closure
reflex :: Ord a => Type a a -> Type a a
reflex r = ( make0 $ do x <- setToList $ source r ; return (x, x) )
	 { source = source r
	 , target = source r -- ??
	 }

-- | reflexive and transitive closure
reflex_trans :: Ord a => Type a a -> Type a a
reflex_trans r = plus (reflex r) (trans r) 

minima :: Ord a => Type a a -> Set a
minima r = source r `minusSet` mkSet ( do (x,y) <- pairs r ; return y )

maxima :: Ord a => Type a a -> Set a
maxima r = target r `minusSet` mkSet ( do (x,y) <- pairs r ; return x )
