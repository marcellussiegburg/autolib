module Autolib.Graph.Knight where

--  $Id$

import Autolib.Graph.Graph 
import Autolib.Graph.Basic
import Autolib.Graph.Ops
import Autolib.Graph.Display

import Autolib.Sets
import Autolib.Util.Teilfolgen
import Control.Monad ( guard )
import Autolib.ToDoc

knight :: Int -> Int -> Graph (Int, Int)
knight w h 
    = informed ( text "Knight" <+> toDoc (w,h) )	
    $ texinformed ( "{\\mathrm{Knight}(" ++ show w ++ "," ++ show h ++ ")}" )
    $ let v = cross ( mkSet [1 .. w] ) ( mkSet [ 1 .. h ] )
	  e = mkSet $ do
	       [u@(a,b),v@(c,d)] <- teilfolgen 2 $ setToList v
	       guard $ 5 == (a-c)^2 + (b-d)^2
	       return $ kante u v
      in mkGraph v e


