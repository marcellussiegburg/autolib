module Autolib.Graph.Ops where

-- -- $Id$

import Autolib.Graph.Graph hiding ( union )

import Autolib.Graph.Basic
import Autolib.Graph.Display

import Autolib.Boxing hiding ( grid )
import Autolib.ToDoc hiding (empty)
import Autolib.Set as Set


import qualified Data.List as List
import Autolib.FiniteMap
import Data.Maybe
import Control.Monad ( guard )
import Autolib.Util.Teilfolgen

--------------------------------------------------------------------------

instance GraphC a => Boxing (Graph a) where
    bounding_box = bounding
    set_bounding_box b g = g { bounding = b }

    pack ogs = unions0 $ do
         (off, g) <- ogs
	 let fm = mapFM ( \ v p -> off + p ) $ graph_layout g
	 return $ g { graph_layout = fm }


--------------------------------------------------------------------------

-- | durch nicht injektive mappen
-- kann man hier auch kontrahieren
-- TODO: das layout geht dann aber kaputt 
gmap :: ( GraphC a, GraphC b ) => (a -> b ) -> ( Graph a -> Graph b )
gmap f g = g
	 { knoten = smap f $ knoten g
	 , kanten = smap ( \ k -> kante (f $ von k) (f $ nach k) )
				   $ kanten g
	 , graph_layout = listToFM $ do
	       ( v, p ) <- fmToList $ graph_layout g
	       return ( f v, p ) -- bei kontraktionen mittelwert bilden?
	 }


normalize :: ( GraphC Int, GraphC a ) 
	  => Graph a -> Graph Int
normalize g =
    let fm = listToFM $ zip ( setToList $ knoten g ) [ 0 .. ]
	fun = fromMaybe ( error "Graph.Ops.normalize" ) . lookupFM fm
    in	gmap fun g

--------------------------------------------------------------------------

complement :: GraphC a => Graph a -> Graph a
complement g = informed ( funni "co" [ info g ] )
	     $ texinformed ( "\\overline{" ++ texinfo g ++ "}" )
	     $ clique ( knoten g ) `unlinks` setToList ( kanten g )

-- | klebt zusammen (zeichnet übereinander!)
union0 :: GraphC a => Graph a -> Graph a -> Graph a
union0 g1 g2 = g1 -- use layout prog and hints from g1
	    { graph_info = funni "union0" [ info g1, info g2 ] 
	    , graph_texinfo = texinfo g1 ++ "\\cup" ++ texinfo g2
	    , knoten = Set.union (knoten g1) (knoten g2)
	    , kanten = Set.union (kanten g1) (kanten g2)
	    , graph_layout = plusFM (graph_layout g1) (graph_layout g2)
	    , bounding = larger (bounding g1) (bounding g2)
	    }

unions0 :: GraphC a => [ Graph a ] -> Graph a
unions0 = foldr union0 empty

-------------------------------------------------------------------------

-- | bilder nebeneinander, aber knotenmengen gemeinsam
union1 ::  GraphC a => Graph a -> Graph a -> Graph a
union1 g1 g2 = informed ( funni "union" [ info g1, info g2 ] )
	     $ texinformed (  texinfo g1 ++ "+" ++ texinfo g2 )
	     $ Autolib.Boxing.beside g1 g2

-- | richtig disjunkte union
union  :: ( GraphC a, GraphC b, GraphC ( Either a b ) ) 
       => Graph a -> Graph b -> Graph (Either a b)
union g1 g2 = union1 (gmap Left g1) (gmap Right g2)

-- | knotenmengen gemeinsam (benutzt in partit)
times0 :: (GraphC a) 
       => Graph a -> Graph a -> Graph a
times0 l r = informed ( funni "times" [ info l, info r ] )
	  $ texinformed (  texinfo l ++ "*" ++ texinfo r )
	  $ union0 l r  `links0` do
              u <- setToList $ knoten l 
	      v <- setToList $ knoten r
	      return $ kante u v

-- | knotenmengen disjunkt machen
times ::  (GraphC a, GraphC b, GraphC ( Either a b ) ) 
      => Graph a -> Graph b -> Graph (Either a b)
times l r = times0 ( gmap Left l ) ( gmap Right r )

partit :: ( ToDoc a, GraphC a)
        => [Set a] -> Graph a
partit xss = foldr1 times0 $ map independent xss

---------------------------------------------------------------------------

grid :: ( GraphC a, GraphC b, GraphC (a, b) ) 
     => Graph a -> Graph b
     -> Graph (a, b)
-- gibt es dafür einen namen? 
-- es ist nicht das lexikografische produkt.
grid l r = informed ( funni "grid" [ info l, info r ] )
	     $ texinformed (  texinfo l ++ "\\times" ++ texinfo r )
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

restrict :: ( ToDoc [a], GraphC a )
         => Set a -> Graph a 
	 -> Graph a
restrict xs g = 
    let knok x = x `elementOf` xs
	kaok k = all knok [ von k, nach k ]
    in  informed ( funni "restrict" [ toDoc xs, info g ] )
	      $ g { knoten = sfilter knok $ knoten g
		  , kanten = sfilter kaok $ kanten g
		  }

---------------------------------------------------------------------------

-- | addiert eine kante
link :: ( ToDoc a, GraphC a) 
     => Graph a -> Kante a -> Graph a
link g k = informed ( fsep [info g, text "+" ,toDoc (von k), toDoc( nach k) ] )
         $ link0 g k

link0 :: GraphC a => Graph a -> Kante a -> Graph a
link0 g k = g { kanten = Set.union (kanten g) (Set.unitSet k) }

-- | addiert mehrere kanten
links ::  ( ToDoc a, GraphC a)
       => Graph a -> [ Kante a ] -> Graph a
links g ks = informed ( fsep [info g, text "+" , toDoc ks ] )
           $ links0 g ks 

links0 :: GraphC a => Graph a -> [Kante a] -> Graph a
links0 = foldl link0


-- | entfernt kante
unlink :: GraphC a => Graph a -> Kante a  -> Graph a
unlink g k = g { kanten = kanten g `Set.minusSet` unitSet k }

-- | entfernt kante
unlinks :: GraphC a => Graph a -> [ Kante a ]  -> Graph a
unlinks g ks = g { kanten = kanten g `Set.minusSet` mkSet ks }

--------------------------------------------------------------------
