module Autolib.Dot.Arrange where

-- -- $Id$

import qualified Autolib.Dot.Graph
import qualified Autolib.Dot.Node
import qualified Autolib.Dot.Edge
import Autolib.Dot.Dot
import qualified Autolib.GVKnoten.Layout
import Autolib.Boxing.Position

import Data.Set
import Control.Monad ( foldM )
import Random
import Data.FiniteMap
import IO

-- koordinaten der knoten bestimmen.
-- dabei bereits feststehende koordinaten so lassen (pinning)
-- geht nur mit neato (?)

debug msg = do
    -- hPutStrLn stderr msg
    return ()

newtype Self = Self { unSelf :: Autolib.Dot.Graph.Type }

instance ToDot Self where
    toDot        = unSelf
    toDotProgram = const "neato"
    toDotOptions = const "-s" 

arrange :: Autolib.Dot.Graph.Type 
	-> IO ( FiniteMap String Position )
arrange g = do

    debug $ "arrange: " ++ show g

    n <- randomRIO (0,10000 :: Int)
    let fname = "/tmp/arrange." ++ show n
    dotfile <- Autolib.Dot.Dot.mot "" ".out" fname $ Self g
    Autolib.GVKnoten.Layout.get_positions dotfile

layered :: Autolib.Dot.Graph.Type
	-> [ Set String ]
	-> IO Autolib.Dot.Graph.Type
-- der reihe nach die jeweiligen induzierten teilgraphen layouten
layered g xss = do
    debug $ "layered.g: " ++ show g
    debug $ "layered.xss: " ++ show xss
    fm <- foldM ( \ fm xs -> arrange $ beef fm $ restrict g xs ) 
	        emptyFM xss
    return $ beef fm g

restrict :: Autolib.Dot.Graph.Type
         -> Set String
	 -> Autolib.Dot.Graph.Type
-- Teilgraph, der von xs induziert wird
restrict g xs = 
    let knok x = x `elementOf` xs
	kaok k = all knok [ Autolib.Dot.Edge.from k, Autolib.Dot.Edge.to k ]
    in  g { Autolib.Dot.Graph.nodes = filter ( knok. Autolib.Dot.Node.ident ) 
	                    $ Autolib.Dot.Graph.nodes g
	  , Autolib.Dot.Graph.edges = filter kaok $ Autolib.Dot.Graph.edges g
	  }

-- füge errechnete koordinaten hinzu
beef :: FiniteMap String Position 
     -> Autolib.Dot.Graph.Type
     -> Autolib.Dot.Graph.Type
beef fm g 
    = Autolib.Dot.Graph.scale 1.3 -- ?? guess
    $ g { Autolib.Dot.Graph.nodes = do
	    n <- Autolib.Dot.Graph.nodes g
	    return $ case lookupFM fm ( Autolib.Dot.Node.ident n ) of
	                  Just pos -> n { Autolib.Dot.Node.pinned = Just True
					, Autolib.Dot.Node.position = Just pos
					}
	                  Nothing  -> n
      }

