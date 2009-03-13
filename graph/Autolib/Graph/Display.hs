{-# language FlexibleInstances, OverlappingInstances, IncoherentInstances #-}

module Autolib.Graph.Display 

--  $Id$

( module Autolib.Dot.Dot
, dot_numbered
)

where

import Autolib.Graph.Graph
import Autolib.Dot.Dot

import qualified Autolib.Dot.Graph
import qualified Autolib.Dot.Node
import qualified Autolib.Dot.Edge

import Autolib.FiniteMap
import Data.Maybe
import Control.Monad ( liftM )

-------------------------------------------------------------------------------

-- | einfacher graph
instance ( GraphC a ) => ToDot ( Graph a ) where
    toDotProgram = layout_program
    toDot g =
        let vs = setToList $ knoten g
	    fm = listToFM $ zip vs $ [ 0 :: Int .. ] 
            num = fromMaybe (error "Graph.Display.num") . lookupFM fm
	in  dot_numbered g num
    toDotOptions = unwords . layout_hints 

-- | graphen mit kantengewichten
instance ( GraphC a, Show b ) => ToDot ( Graph a , Kante a -> b ) where
    toDotProgram = layout_program . fst
    toDot (g,w) =
        let vs = setToList $ knoten g
	    fm = listToFM $ zip vs $ [ 0 :: Int .. ] 
            num = fromMaybe (error "Graph.Display.num") . lookupFM fm
	in  dot_weight_numbered g num ( Just . show . w )
    toDotOptions = unwords . layout_hints . fst

-- | graphen mit vielleicht kantengewichten
instance ( GraphC a,  Show b ) 
    => ToDot ( Graph a , Kante a -> Maybe b ) where
    toDotProgram = layout_program . fst
    toDot (g,w) =
        let vs = setToList $ knoten g
	    fm = listToFM $ zip vs $ [ 0 :: Int .. ] 
            num = fromMaybe (error "Graph.Display.num") . lookupFM fm
	in  dot_weight_numbered g num ( liftM show . w )
    toDotOptions = unwords . layout_hints . fst

-- | graphen mit vielleicht kantengewichten und vielleicht kantenstilen
instance ( GraphC a , Show b )
    => ToDot ( Graph a , Kante a -> Maybe b , Kante a -> Maybe String ) where
    toDotProgram (g,_,_) = layout_program g
    toDot (g,w,s) =
        let vs = setToList $ knoten g
	    fm = listToFM $ zip vs $ [ 0 :: Int .. ] 
            num = fromMaybe (error "Graph.Display.num") . lookupFM fm
	in  dot_numbered' g num ( liftM show . w ) s
    toDotOptions (g,_,_) = unwords $ layout_hints g

-- | zwei graphen: der zweite wird benutzt, um kanten des ersten hervorzuheben
instance ( GraphC a , Show b )
    => ToDot ( Graph a , Graph a , Kante a -> Maybe b ) where
    toDotProgram (g,_,_) = layout_program g
    toDotOptions (g,_,_) = unwords $ layout_hints g
    toDot (x,y,w) = toDot ( x 
			  , \ k -> if k `elementOf` (kanten y)
			           then w k
			           else Nothing
			  , \ k -> if k `elementOf` (kanten y) 
			           then Just "solid" 
			           else Just "dotted" 
			  )

-------------------------------------------------------------------------------


dot_numbered :: ( GraphC a, Show b ) 
	     => Graph a -> ( a -> b ) -> Autolib.Dot.Graph.Type
dot_numbered g num = dot_weight_numbered g num (\_->Nothing)

dot_weight_numbered :: ( GraphC a, Show b ) 
	            => Graph a 
		    -> ( a -> b ) 
		    -> ( Kante a -> Maybe String )
		    -> Autolib.Dot.Graph.Type
dot_weight_numbered g num w = dot_numbered' g num w (\_->Nothing)

dot_numbered' :: ( GraphC a, Show b ) 
	      => Graph a 
	      -> ( a -> b ) 
	      -> ( Kante a -> Maybe String ) -- label
	      -> ( Kante a -> Maybe String ) -- style
	      -> Autolib.Dot.Graph.Type
dot_numbered' g num w s =
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

            es = do k@Kante { von = p, nach = q } <- setToList $ kanten g
                    return $ Autolib.Dot.Edge.blank
                           { Autolib.Dot.Edge.from  = show $ num p
                           , Autolib.Dot.Edge.to    = show $ num q
			   , Autolib.Dot.Edge.directed = False
			   , Autolib.Dot.Edge.label = w k
			   , Autolib.Dot.Edge.edge_style = s k
                           }

        in  Autolib.Dot.Graph.Type 
            { Autolib.Dot.Graph.directed = False
            , Autolib.Dot.Graph.name = "foo"
            , Autolib.Dot.Graph.nodes = ns 
            , Autolib.Dot.Graph.edges = es 
            , Autolib.Dot.Graph.attributes = []
            }
