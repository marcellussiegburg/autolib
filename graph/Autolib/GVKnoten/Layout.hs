module GVKnoten.Layout where

-- $Id$

import Graph.Graph
import Graph.Display

import GVKnoten.Type
import GVKnoten.Parser

import Set
import FiniteMap
import Maybe

import Random
import System

-- in der eingabe kˆnnen einige knoten schon gepinnt sein
-- dazu benutze graph.graph_layout

layout :: ( Ord a, Show a ) 
       => Graph a -> IO (FiniteMap a Pos)
layout graph = do

    let 
        fm = listToFM $ zip ( setToList $ knoten graph ) [ 1 .. ]
	mf = invert fm
        gn = dot_numbered graph ( look fm )

    n <- randomRIO (0,10000 :: Int)
    let prefix = "/tmp/layout" ++ show n
	infile = prefix ++ ".in"
	outfile = prefix ++ ".out"

    writeFile infile $ show gn
    system $ unwords [ "neato", "-o", outfile , infile ]
    gvknoten <- parse outfile
    system $ unwords [ "rm", infile, outfile ]

    let positions = addListToFM_C 
		    ( error "GVKnoten.Layout.positions" ) emptyFM $ do 
	    v <- gvknoten
	    return ( look mf $ read $ ident v , pos v )
    return positions


invert :: ( Ord a, Ord b ) => FiniteMap a b -> FiniteMap b a
invert fm = addListToFM_C (error "GVKnoten.Layout.invert") emptyFM $ do
       ( x, y) <- fmToList fm ; return ( y, x )


look :: Ord a => FiniteMap a b -> a -> b
-- argument muﬂ drin sein
look fm = fromMaybe ( error "GVKnoten.Layout.look" ) . lookupFM fm
