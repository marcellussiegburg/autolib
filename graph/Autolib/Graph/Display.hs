module Autolib.Graph.Display 

-- -- $Id$

( module Autolib.Dot.Dot
, dot_numbered
)

where

import Autolib.Graph.Graph
import Autolib.Dot.Dot

import qualified Autolib.Dot.Graph
import qualified Autolib.Dot.Node
import qualified Autolib.Dot.Edge

import Autolib.Boxing.Position

import Data.FiniteMap
import Data.Maybe
import Control.Monad ( guard )

instance ( Show a, Ord a ) => ToDot ( Graph a ) where
    toDotProgram = layout_program
    toDot g =
        let vs = setToList $ knoten g
	    fm = listToFM $ zip vs $ [ 0 :: Int .. ] 
            num = fromMaybe (error "Graph.Display.num") . lookupFM fm
	in  dot_numbered g num
    toDotOptions = unwords . layout_hints 

dot_numbered :: ( Show a, Ord a, Show b ) 
	     => Graph a -> ( a -> b ) -> Autolib.Dot.Graph.Type
dot_numbered g num = 
    let 
            pins = graph_layout g

	    tricky cs = 
	        if take 1 cs `elem` [ "\"", "'" ]  
		   -- dann ist es Show String|Char
		then tail ( init cs )	  -- und eine "-klammer kann weg
		else cs

            ns = do v <- setToList $ knoten g
                    let n  = Autolib.Dot.Node.blank
                           { Autolib.Dot.Node.ident = show $ num v
                           , Autolib.Dot.Node.label = 
			        if show_labels g
			        then return $ tricky $ show v
			        else return ""
                           }
		    return $ case lookupFM pins v of
		        Nothing -> n
			Just p  -> n { Autolib.Dot.Node.pinned = Just True
				     , Autolib.Dot.Node.position = Just p
				     }

            es = do Kante { von = p, nach = q } <- setToList $ kanten g
                    return $ Autolib.Dot.Edge.blank
                           { Autolib.Dot.Edge.from  = show $ num p
                           , Autolib.Dot.Edge.to    = show $ num q
			   , Autolib.Dot.Edge.directed = False
                           }

        in  Autolib.Dot.Graph.Type 
            { Autolib.Dot.Graph.directed = False
            , Autolib.Dot.Graph.name = "foo"
            , Autolib.Dot.Graph.nodes = ns 
            , Autolib.Dot.Graph.edges = es 
            }


