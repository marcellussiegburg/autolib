module Graph.Knight where

-- $Id$

import Graph.Graph 
import Graph.Basic
import Graph.Ops
import Graph.Display

import Sets
import Util.Teilfolgen
import Monad ( guard )
import ToDoc

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


