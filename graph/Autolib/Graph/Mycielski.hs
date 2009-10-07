module Autolib.Graph.Mycielski where

--  $Id$

import Autolib.Graph.Graph 
import Autolib.Graph.Basic
import Autolib.Graph.Ops
import Autolib.Graph.Display

import Autolib.ToDoc
import Autolib.FiniteMap
import Autolib.Boxing.Position
import Autolib.GVKnoten.Layout

import Autolib.XmlRpc

-- | Mycielsky construction:
-- does not introduce triangles
-- but increases chromatic number by one
mycielski :: ( GraphC a )
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

-- | the Grötzsch graph 'mycielski $ mycielski $ path [1, 2]'
grotzsch :: Graph Int
grotzsch = informed ( text "Grötzsch" )
         $ mycielski $ mycielski $ path [1 :: Int, 2]


