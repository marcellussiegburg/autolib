module Graph.Display 

( module Dot.Dot
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
    toDot g =
        let vs = setToList $ knoten g
	    fm = listToFM $ zip vs $ [ 0.. ] 
            num = fromMaybe (error "Graph.Display.num") . lookupFM fm

	    tricky cs = 
	        if take 1 cs == ['"'] -- dann ist es Show String
		then tail ( init cs )	  -- und eine "-klammer kann weg
		else cs

            ns = do v <- vs
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


