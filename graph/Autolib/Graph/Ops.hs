module Graph.Ops where

-- $Id$

import Graph.Graph hiding ( union )

import Graph.Basic
import Graph.Display

import Boxing
import ToDoc hiding (empty)

import qualified Set
import qualified List
import FiniteMap
import Maybe
import Monad ( guard )
import Util.Teilfolgen

--------------------------------------------------------------------------

instance Ord a => Boxing (Graph a) where
    bounding_box = bounding
    set_bounding_box b g = g { bounding = b }

    pack ogs = unions0 $ do
         (off, g) <- ogs
	 let fm = mapFM ( \ v p -> off + p ) $ graph_layout g
	 return $ g { graph_layout = fm }


--------------------------------------------------------------------------

gmap :: ( Ord a, Ord b ) => (a -> b ) -> ( Graph a -> Graph b )
-- durch nicht injektive mappen
-- kann man hier auch kontrahieren
-- TODO: das layout geht dann aber kaputt 
gmap f g = Graph 
	 { graph_info = info g -- nichts anzeigen
	 , knoten = mapSet f $ knoten g
	 , kanten = mapSet ( \ k -> kante (f $ von k) (f $ nach k) )
				   $ kanten g
	 , graph_layout = listToFM $ do
	       ( v, p ) <- fmToList $ graph_layout g
	       return ( f v, p ) -- bei kontraktionen mittelwert bilden?
	 , bounding = bounding g
    , layout_hints = ""
	 }


normalize :: Ord a => Graph a -> Graph Int
normalize g =
    let fm = listToFM $ zip ( setToList $ knoten g ) [ 0 .. ]
	fun = fromMaybe ( error "Graph.Ops.normalize" ) . lookupFM fm
    in	gmap fun g

--------------------------------------------------------------------------

complement :: Ord a => Graph a -> Graph a
complement g = informed ( funni "co" [ info g ] )
	     $ clique ( knoten g ) `unlinks` setToList ( kanten g )

union0 :: Ord a => Graph a -> Graph a -> Graph a
-- klebt zusammen (zeichnet übereinander!)
union0 g1 g2 = 
      Graph { graph_info = funni "union0" [ info g1, info g2 ] 
	    , knoten = Set.union (knoten g1) (knoten g2)
	    , kanten = Set.union (kanten g1) (kanten g2)
	    , graph_layout = plusFM (graph_layout g1) (graph_layout g2)
	    , bounding = larger (bounding g1) (bounding g2)
    , layout_hints = ""
	    }

unions0 :: Ord a => [ Graph a ] -> Graph a
unions0 = foldr union0 empty

-------------------------------------------------------------------------

union1 ::  Ord a => Graph a -> Graph a -> Graph a
-- bilder nebeneinander, aber knotenmengen gemeinsam
union1 g1 g2 = informed ( funni "union" [ info g1, info g2 ] )
	     $ beside g1 g2

union  :: ( Ord a, Ord b) => Graph a -> Graph b -> Graph (Either a b)
-- richtig disjunkte union
union g1 g2 = union1 (gmap Left g1) (gmap Right g2)

times0 :: (Ord a) 
       => Graph a -> Graph a -> Graph a
-- knotenmengen gemeinsam (benutzt in partit)
times0 l r = informed ( funni "times" [ info l, info r ] )
	  $ union0 l r  `links0` do
              u <- setToList $ knoten l 
	      v <- setToList $ knoten r
	      return $ kante u v

times ::  (Ord a, Ord b ) 
      => Graph a -> Graph b -> Graph (Either a b)
-- knotenmengen disjunkt machen
times l r = times0 ( gmap Left l ) ( gmap Right r )

partit :: ( ToDoc a, Ord a)
        => [Set a] -> Graph a
partit xss = foldr1 times0 $ map independent xss

---------------------------------------------------------------------------

grid :: ( Ord a, Ord b ) 
     => Graph a -> Graph b
     -> Graph (a, b)
-- gibt es dafür einen namen? 
-- es ist nicht das lexikografische produkt.
grid l r = informed ( funni "grid" [ info l, info r ] )
	 $ let vs = cross (knoten l) (knoten r)
	       es = do
	              u <- setToList $ knoten l
		      k <- setToList $ kanten r
		      return $ kante ( u, von k ) ( u, nach k )
		 ++ do     
	              u <- setToList $ knoten r
		      k <- setToList $ kanten l
		      return $ kante ( von k, u ) ( nach k, u )
	   in mkGraph vs (mkSet es)


---------------------------------------------------------------------------

line_graph :: Ord a => Graph a -> Graph ( Kante a )
line_graph g = 
    let v = kanten g
	e = mkSet $ do 
	       [ u, v ] <- teilfolgen 2 $ setToList $ kanten g
	       let inhalt k = mkSet [ von k, nach k ]
	       guard $ not $ isEmptySet $ inhalt u `intersect` inhalt v
	       return $ kante u v
    in Graph { graph_info = funni "L" [ info g ]
	     , knoten = v
	     , kanten = e 
	     , graph_layout = listToFM $ do 
	           k <- setToList v
		   let u = von k
		       v = nach k
		   let pos = lookupWithDefaultFM 
			     (graph_layout g) (error "edge_graph")
		   return ( k, 0.5 * (pos u + pos v) )
	     , bounding = bounding g         
    , layout_hints = ""
	     }	       

---------------------------------------------------------------------------

link :: ( ToDoc a, Ord a) 
     => Graph a -> Kante a -> Graph a
-- addiert eine kante
link g k = informed ( fsep [info g, text "+" ,toDoc (von k), toDoc( nach k) ] )
         $ link0 g k

link0 :: Ord a => Graph a -> Kante a -> Graph a
link0 g k = g { kanten = Set.union (kanten g) (Set.unitSet k) }

links ::  ( ToDoc a, Ord a)
       => Graph a -> [ Kante a ] -> Graph a
-- addiert mehrere kanten
links g ks = informed ( fsep [info g, text "+" , toDoc ks ] )
           $ links0 g ks 

links0 :: Ord a => Graph a -> [Kante a] -> Graph a
links0 = foldl link0


unlink :: Ord a => Graph a -> Kante a  -> Graph a
-- entfernt kante
unlink g k = g { kanten = kanten g `Set.minusSet` unitSet k }

unlinks :: Ord a => Graph a -> [ Kante a ]  -> Graph a
-- entfernt kante
unlinks g ks = g { kanten = kanten g `Set.minusSet` mkSet ks }

--------------------------------------------------------------------
