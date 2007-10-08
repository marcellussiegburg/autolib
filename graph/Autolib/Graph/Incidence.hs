module Autolib.Graph.Incidence where

--  $Id$

import Autolib.Graph.Graph

import Autolib.Set
import Autolib.Util.Teilfolgen
import Autolib.FiniteMap
import Control.Monad ( guard )

incidence_graph :: 
   ( GraphC a
   , GraphC ( Either a ( Kante a ) ) 
   ) => Graph a -> Graph ( Either a ( Kante a ) )
incidence_graph g = 
    let v = smap Left ( knoten g ) `union` smap Right ( kanten g )
	e = mkSet $ do 
	       k <- setToList $ kanten g
	       v <- [ von k, nach k ]
	       return $ kante ( Left v ) ( Right k )
    in    informed ( funni "I" [ info g ] )
	$ texinformed ( "{\\mathrm{I}(" ++ texinfo g ++ ")}" )
	$ mkGraph v e





