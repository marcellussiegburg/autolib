module Autolib.Graph.Kneser where

--  $Id$

import Autolib.Graph.Type
import Autolib.Graph.Basic
import Autolib.Graph.Ops
import Autolib.Graph.Display

import Autolib.Sets
import Autolib.Util.Teilfolgen
import Control.Monad ( guard )
import Autolib.ToDoc

kneser :: Int -> Int -> Graph (Set Int)
kneser t r = informed ( text "Kneser" <+> toDoc (t,r) )
	   $ texinformed ( "{K_{" ++ show t ++ "}^{" ++ show r ++ "}}" )
	   $ let v = teilmengen r $ mkSet [1 .. t ]
	         e = do
		       [u,v] <- teilfolgen 2 v
		       guard $ isEmptySet $ intersect u v
		       return $ kante u v
	     in mkGraph (mkSet v) (mkSet e)

petersen :: Graph Int
petersen = informed ( text "Petersen" )
         $ normalize $ kneser 5 2
