module Autolib.Graph.Mycielski where

--  $Id$

import Autolib.Graph.Graph 
import Autolib.Graph.Basic
import Autolib.Graph.Ops
import Autolib.Graph.Display

import Autolib.ToDoc
import Data.FiniteMap
import Autolib.Boxing.Position
import Autolib.GVKnoten.Layout

mycielski :: ( Ord a )
	  => Graph a 
          -> Graph Int
mycielski g = informed ( funni "mycielski" [ info g ] )
    $ normalize 
    $ Autolib.Graph.Ops.union 
	 g ( times ( independent $ knoten g) ( independent $ unitSet () ) )
    `links0` do 
	 k <- setToList $ kanten g
	 (u, v) <- [(von k, nach k), (nach k, von k)]
	 return $ kante (Left u) (Right (Left v))

grotzsch :: Graph Int
grotzsch = informed ( text "Grötzsch" )
         $ mycielski $ mycielski $ path [1, 2]


