module Autolib.NFA.Hull

-- -- $Id$

( hull
)

where

import Autolib.Set



hull :: (Ord c, Ord s) 
     => [ c ] -> (s -> c -> s) -> s -> [ ((s, c), s) ]
hull alpha next start = inner emptySet [ start ] where
    inner done [] = []
    inner done (x : todo) =
       let done' = union done ( unitSet x )
	   here = do c <- alpha; return ((x, c), next x c)
	   todo' = filter ( not . ( `elementOf` done' ) )
			   ( map snd here ++ todo ) 
       in  here ++ inner done' todo'


