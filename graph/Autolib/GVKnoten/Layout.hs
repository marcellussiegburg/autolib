module GVKnoten.Layout where

-- $Id$

import Graph.Graph
import Graph.Display
import Graph.Ops ( restrict )
import qualified Boxing.Position as B

import GVKnoten.Type
import GVKnoten.Parser

import ToDoc
import Set
import FiniteMap
import Maybe
import Monad ( foldM )

import Random
import System

-- in der eingabe können einige knoten schon gepinnt sein
-- dazu benutze graph.graph_layout

layout :: ( Ord a, Show a ) 
       => Graph a 
       -> IO (FiniteMap a B.Position)
layout graph = do

    let 
        fm = listToFM $ zip ( setToList $ knoten graph ) [ 1 .. ]
	mf = invert fm
        gn = dot_numbered ( graph   ) ( look fm )

    n <- randomRIO (0,10000 :: Int)
    let prefix = "/tmp/layout" ++ show n
	infile = prefix ++ ".in"
	outfile = prefix ++ ".out"

    writeFile infile $ show gn
    system $ unwords [ "neato", "-s", "-o", outfile , infile ]
    gvknoten <- parse outfile
    system $ unwords [ "rm", infile, outfile ]

    -- todo: hier fehlt noch eine skalierung? oder nicht?
    let f = 0.017
        convert ( x, y ) = B.Position { B.width  = f * fromIntegral x
				      , B.height = f * fromIntegral y 
				      }
    let positions = addListToFM_C 
		    ( error "GVKnoten.Layout.positions" ) emptyFM $ do 
	    v <- gvknoten
	    return ( look mf $ read $ ident v 
		   , convert $ pos v )
    return positions

---------------------------------------------------------------------------

layered_layout :: ( Ord a, Show a, ToDoc [a] )
	       => Graph a -> [ Set a ] 
	       -> IO ( FiniteMap a B.Position )
-- idee: mit layers [ l1, l2, .. ]
-- erst layout nur für teilgraphen auf l1,
-- dann diese punkte fixieren, und layout für l2, usw.
-- d. h.  layered_layout g [knoten g] == layout g   (alle auf einmal)

layered_layout g layers = do
    h <- foldM layer ( g { graph_layout = emptyFM } ) layers
    return $ graph_layout h

layer :: ( Ord a, Show a, ToDoc [a] )
      => Graph a -> Set a 
      -> IO ( Graph a )
layer g xs = do
    let done = mkSet $ keysFM $ graph_layout g
    fm <- layout $ restrict (union xs done) g
    print fm
    return $ g { graph_layout = fm }

---------------------------------------------------------------------------

invert :: ( Ord a, Ord b ) => FiniteMap a b -> FiniteMap b a
invert fm = addListToFM_C (error "GVKnoten.Layout.invert") emptyFM $ do
       ( x, y) <- fmToList fm ; return ( y, x )


look :: Ord a => FiniteMap a b -> a -> b
-- argument muß drin sein
look fm = fromMaybe ( error "GVKnoten.Layout.look" ) . lookupFM fm
