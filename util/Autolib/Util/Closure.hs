module Autolib.Util.Closure where

-- -- $Id$

import Autolib.Sets
import Autolib.Util.Splits
import Autolib.Util.Uniq

unary_binary :: Ord a
       => ( a -> [ a ] )
       -> ( a -> a -> [ a ] )
       -> [ a ]
       -> [ a ]
unary_binary un bin start = xs 
    where xs = uniqs $ start ++ 
            do ( ys, z : _ ) <- splits xs
	       un z ++ do
	           ( l, r ) <- (z, z) : do y <- ys ; [ ( y, z), (z, y) ]
		   bin l r

    