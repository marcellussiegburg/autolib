module Graph.Display 

( module Dot.Dot
, dot_numbered
)

where

import Graph.Graph
import Dot.Dot

import qualified Dot.Graph
import qualified Dot.Node
import qualified Dot.Edge

import FiniteMap
import Maybe

instance ( Show a, Ord a ) => ToDot ( Graph a ) where
    toDotProgram g = "neato"
--    toDotProgram g = "dot"
    toDot g =
        let vs = setToList $ knoten g
	    fm = listToFM $ zip vs $ [ 0 :: Int .. ] 
            num = fromMaybe (error "Graph.Display.num") . lookupFM fm
	in  dot_numbered g num

dot_numbered :: ( Show a, Ord a, Show b ) 
	     => Graph a -> ( a -> b ) -> Dot.Graph.Type
dot_numbered g num = 
    let 
	    tricky cs = 
	        if take 1 cs `elem` [ "\"", "'" ]  
		   -- dann ist es Show String|Char
		then tail ( init cs )	  -- und eine "-klammer kann weg
		else cs

            ns = do v <- setToList $ knoten g
                    return $ Dot.Node.blank
                           { Dot.Node.ident = show $ num v
                           , Dot.Node.label = Just $ tricky $ show v
                           }

            es = do Kante { von = p, nach = q } <- setToList $ kanten g
                    return $ Dot.Edge.blank
                           { Dot.Edge.from  = show $ num p
                           , Dot.Edge.to    = show $ num q
			   , Dot.Edge.directed = False
                           }

        in  Dot.Graph.Type 
            { Dot.Graph.directed = False
            , Dot.Graph.name = "foo"
            , Dot.Graph.nodes = ns 
            , Dot.Graph.edges = es 
            }


