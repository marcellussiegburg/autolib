module Graph.Mycielski where

-- $Id$

import Graph.Graph 
import Graph.Basic
import Graph.Ops
import Graph.Display

import ToDoc
import FiniteMap
import Boxing.Position

mycielski :: ( Ord a )
	  => Graph a 
          -> Graph Int
mycielski g = informed ( funni "mycielski" [ info g ] )
    $ normalize 
    $ Graph.Ops.union 
	 g ( times ( independent $ knoten g) ( independent $ unitSet () ) )
    `links0` do 
	 k <- setToList $ kanten g
	 (u, v) <- [(von k, nach k), (nach k, von k)]
	 return $ kante (Left u) (Right (Left v))

grotzsch :: Graph Int
grotzsch = informed ( text "Grötzsch" )
         $ mycielski $ mycielski $ path [1, 2]


