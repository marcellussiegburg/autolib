module Util.Closure where

-- $Id$

import Sets
import Util.Splits
import Util.Uniq

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

    