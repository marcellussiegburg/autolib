module Graph.Ops where

import Graph.Graph hiding ( union )
import Graph.Display

import qualified Set
import qualified List

clique :: Ord a => Set a -> Graph a
clique xs = Graph { knoten = xs
		  , kanten = mkSet $ do 
			 (x : ys) <- List.tails $ setToList xs
			 y <- ys
			 return $ kante x y
		  }

empty :: Ord a => Graph a
empty = clique $ Set.emptySet


union :: Ord a => Graph a -> Graph a -> Graph a
-- klebt zusammen
union g1 g2 = Graph { knoten = Set.union (knoten g1) (knoten g2)
		    , kanten = Set.union (kanten g1) (kanten g2)
		    }

unions :: Ord a => [ Graph a ] -> Graph a
unions = foldr union empty

link :: Ord a => Graph a -> Kante a -> Graph a
-- addiert eine kante
link g k = g { kanten = Set.union (kanten g) (Set.unitSet k) }

links ::  Ord a => Graph a -> [ Kante a ] -> Graph a
-- addiert mehrere kanten
links = foldl link