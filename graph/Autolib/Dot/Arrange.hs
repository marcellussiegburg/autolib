module Dot.Arrange where

-- -- $Id$

import qualified Dot.Graph
import qualified Dot.Node
import qualified Dot.Edge
import Dot.Dot
import qualified GVKnoten.Layout
import Boxing.Position

import Set
import Monad ( foldM )
import Random
import FiniteMap
import IO

-- koordinaten der knoten bestimmen.
-- dabei bereits feststehende koordinaten so lassen (pinning)
-- geht nur mit neato (?)

debug msg = do
    -- hPutStrLn stderr msg
    return ()

newtype Self = Self { unSelf :: Dot.Graph.Type }

instance ToDot Self where
    toDot        = unSelf
    toDotProgram = const "neato"
    toDotOptions = const "-s" 

arrange :: Dot.Graph.Type 
	-> IO ( FiniteMap String Position )
arrange g = do

    debug $ "arrange: " ++ show g

    n <- randomRIO (0,10000 :: Int)
    let fname = "/tmp/arrange." ++ show n
    dotfile <- Dot.Dot.mot "" ".out" fname $ Self g
    GVKnoten.Layout.get_positions dotfile

layered :: Dot.Graph.Type
	-> [ Set String ]
	-> IO Dot.Graph.Type
-- der reihe nach die jeweiligen induzierten teilgraphen layouten
layered g xss = do
    debug $ "layered.g: " ++ show g
    debug $ "layered.xss: " ++ show xss
    fm <- foldM ( \ fm xs -> arrange $ beef fm $ restrict g xs ) 
	        emptyFM xss
    return $ beef fm g

restrict :: Dot.Graph.Type
         -> Set String
	 -> Dot.Graph.Type
-- Teilgraph, der von xs induziert wird
restrict g xs = 
    let knok x = x `elementOf` xs
	kaok k = all knok [ Dot.Edge.from k, Dot.Edge.to k ]
    in  g { Dot.Graph.nodes = filter ( knok. Dot.Node.ident ) 
	                    $ Dot.Graph.nodes g
	  , Dot.Graph.edges = filter kaok $ Dot.Graph.edges g
	  }

-- füge errechnete koordinaten hinzu
beef :: FiniteMap String Position 
     -> Dot.Graph.Type
     -> Dot.Graph.Type
beef fm g 
    = Dot.Graph.scale 1.3 -- ?? guess
    $ g { Dot.Graph.nodes = do
	    n <- Dot.Graph.nodes g
	    return $ case lookupFM fm ( Dot.Node.ident n ) of
	                  Just pos -> n { Dot.Node.pinned = Just True
					, Dot.Node.position = Just pos
					}
	                  Nothing  -> n
      }

