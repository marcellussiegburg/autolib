module GVKnoten.Layout where

-- -- $Id$

import Graph.Graph
import Graph.Display
import Graph.Ops ( restrict )
import qualified Boxing.Position as B

import GVKnoten.Type
import GVKnoten.Parser

import ToDoc
import Data.Set
import Data.FiniteMap
import Maybe
import Control.Monad ( foldM )

import qualified Dot.Node
import qualified Dot.Graph

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
    system $ unwords [ "rm", infile ]
    pos <- get_positions outfile
    return $ listToFM $ do
        ( id, p ) <- fmToList pos
	return ( look mf $ read id, p )

get_positions :: FilePath -> IO ( FiniteMap String B.Position )
get_positions outfile = do
    cs <- readFile outfile
    system $ unwords [ "rm", outfile ]
    let g = read cs :: Dot.Graph.Type
    return $ listToFM $ do 
	  n <- Dot.Graph.nodes g
	  Just p <- return $ Dot.Node.position n
	  return ( Dot.Node.ident n, p )

get_positions_stefan :: FilePath -> IO ( FiniteMap String B.Position )
get_positions_stefan outfile = do

    gvknoten <- parse outfile
    system $ unwords [ "rm", outfile ]

    -- todo: hier fehlt noch eine skalierung? oder nicht?
    let f = 0.017
        convert ( x, y ) = B.Position { B.width  = f * fromIntegral x
				      , B.height = f * fromIntegral y 
				      }
    let positions = addListToFM_C 
		    ( error "GVKnoten.Layout.positions" ) emptyFM $ do 
	    v <- gvknoten
	    return ( read $ ident v 
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
    -- print fm
    return $ g { graph_layout = fm }

---------------------------------------------------------------------------

invert :: ( Ord a, Ord b ) => FiniteMap a b -> FiniteMap b a
invert fm = addListToFM_C (error "GVKnoten.Layout.invert") emptyFM $ do
       ( x, y) <- fmToList fm ; return ( y, x )


look :: Ord a => FiniteMap a b -> a -> b
-- argument muß drin sein
look fm = fromMaybe ( error "GVKnoten.Layout.look" ) . lookupFM fm
