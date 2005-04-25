-- $Id$

-- | (minimale) (auf-)spannende bäume nach algorithmus von prim

module Autolib.Graph.SpanTree 

( st       -- Ord a => Graph a -> Graph a
, mst      -- ( Ord a , Ord b ) => Graph a -> Weight (Kante a) b -> Graph a
, msts     -- ( Ord a , Ord b ) => Graph a -> Weight (Kante a) b -> [Graph a]
, weight   -- Num b => Graph a -> Weight (Kante a) b -> b
, Weight   -- type Weight a b = a -> b
)

where

import Autolib.Graph.Type ( Graph , knoten , kanten , Kante (..) )
import Autolib.Set ( Set , setToList , elementOf , unitSet , addToSet 
		   , delFromSet , emptySet , isEmptySet
		   )

import Control.Monad ( guard )

-------------------------------------------------------------------------------

type Weight a b = a -> b

-- | gesamtgewicht aller kanten
weight :: Num b => Graph a -> Weight (Kante a) b -> b
weight g w = sum $ map w $ setToList $ kanten g

-- | ein minimaler spannender baum bzgl. der gewichtsfunktion
mst :: ( Ord a , Ord b ) => Graph a -> Weight (Kante a) b -> Graph a
mst g = head . msts g

-- | ein spannender baum
st :: Ord a => Graph a -> Graph a
st g = head $ msts g ( \ _ -> 1 :: Int )
 
-------------------------------------------------------------------------------

-- | alle msts, die der algorithmus von prim von den verschiedenen startknoten
-- | ausgehend findet, die sind i.A. nicht paarweise voneinander verschieden

msts :: ( Ord a , Ord b ) => Graph a -> Weight (Kante a) b -> [Graph a]
msts g w = concatMap (msts_from g w) $ setToList $ knoten g

-------------------------------------------------------------------------------

msts_from :: ( Ord a , Ord b ) => Graph a -> Weight (Kante a) b -> a -> [Graph a]
msts_from g w a = build (unitSet a) emptySet (delFromSet (knoten g) a)

    where kl = setToList $ kanten g

	  build _ b v | isEmptySet v = [g { kanten = b }]
	  build m b v = let cs = connectors m v kl
                        in concat $ do 
			   (k,(_,y)) <- min_weights ( w . fst ) cs
			   return $ build (addToSet   m y) 
				          (addToSet   b k) 
				          (delFromSet v y)

connectors :: Ord a => Set a -> Set a -> [Kante a] -> [(Kante a,(a,a))]
connectors ls rs ks = do k <- ks
			 (is,lr) <- is_connector ls rs k
			 guard is
			 return (k,lr)

is_connector :: Ord a => Set a -> Set a -> Kante a -> [(Bool,(a,a))]
is_connector ls rs (Kante { von = v , nach = n }) =
	     [ ( and [ v `elementOf` ls , n `elementOf` rs ] , (v,n) )
	     , ( and [ n `elementOf` ls , v `elementOf` rs ] , (n,v) )
	     ]

min_weights :: Ord b => Weight a b -> [a] -> [a]
min_weights _ [] = []
min_weights w (k:ks) = min_from (w k) [k] ks
    where min_from _ ms [] = ms
	  min_from m ms (x:xs) = let m' = w x
                                 in case compare m' m of
				     LT -> min_from m' [x] xs
				     EQ -> min_from m (x:ms) xs
				     GT -> min_from m ms xs
