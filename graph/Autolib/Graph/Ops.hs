module Graph.Ops where

-- $Id$

import Graph.Graph hiding ( union )

import Graph.Basic
import Graph.Display

import qualified Set
import qualified List
import FiniteMap
import Maybe
import Monad ( guard )


gmap :: ( Ord a, Ord b ) => (a -> b ) -> ( Graph a -> Graph b )
gmap f g = Graph { knoten = mapSet f $ knoten g
		 , kanten = mapSet ( \ k -> kante (f $ von k) (f $ nach k) )
				   $ kanten g
		 }


normalize :: Ord a => Graph a -> Graph Int
normalize g =
    let fm = listToFM $ zip ( setToList $ knoten g ) [ 0 .. ]
	fun = fromMaybe ( error "Graph.Ops.normalize" ) . lookupFM fm
    in	gmap fun g

--------------------------------------------------------------------------

complement :: Ord a => Graph a -> Graph a
complement g = 
    clique ( knoten g ) `unlinks` setToList ( kanten g )

union :: Ord a => Graph a -> Graph a -> Graph a
-- klebt zusammen
union g1 g2 = Graph { knoten = Set.union (knoten g1) (knoten g2)
		    , kanten = Set.union (kanten g1) (kanten g2)
		    }

unions :: Ord a => [ Graph a ] -> Graph a
unions = foldr union empty

times :: ( Ord a, Ord b ) => Graph a -> Graph b -> Graph (Either a b)
times l r = times0 ( gmap Left l ) ( gmap Right r )

times0 :: ( Ord a ) => Graph a -> Graph a -> Graph a
times0 l r = union l r `links` do 
          u <- setToList $ knoten l 
	  v <- setToList $ knoten r
	  return $ kante u v

partit :: Ord a => [Set a] -> Graph a
partit xss = foldr times0 empty $ map independent xss

edge_graph :: Ord a => Graph a -> Graph ( Kante a )
edge_graph g = 
    Graph { knoten = kanten g
	  , kanten = mkSet $ do 
	       [ u, v ] <- teilfolgen 2 $ setToList $ kanten g
	       let inhalt k = mkSet [ von k, nach k ]
	       guard $ not $ isEmptySet $ inhalt u `intersect` inhalt v
	       return $ kante u v
	  }	       

---------------------------------------------------------------------------

link :: Ord a => Graph a -> Kante a -> Graph a
-- addiert eine kante
link g k = g { kanten = Set.union (kanten g) (Set.unitSet k) }

links ::  Ord a => Graph a -> [ Kante a ] -> Graph a
-- addiert mehrere kanten
links = foldl link


unlink :: Ord a => Graph a -> Kante a  -> Graph a
-- entfernt kante
unlink g k = g { kanten = kanten g `Set.minusSet` unitSet k }

unlinks :: Ord a => Graph a -> [ Kante a ]  -> Graph a
-- entfernt kante
unlinks g ks = g { kanten = kanten g `Set.minusSet` mkSet ks }

--------------------------------------------------------------------


teilfolgen :: Int -> [a] -> [[a]]
teilfolgen k xs | k > length xs = []
teilfolgen 0 xs = [[]]
teilfolgen k (x : xs) 
    =  teilfolgen k xs
    ++ do ys <- teilfolgen (k-1) xs ; return $ x : ys
