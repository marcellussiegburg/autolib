module Schichten

( schichten 
, schichten'
)

where

import Set

schichten :: Ord a => (a -> Set a) -> a -> [ Set a ]
schichten f x0 = schichten' f (unitSet x0)

schichten' :: Ord a => (a -> Set a) -> (Set a) -> [ Set a ]
schichten' f xs = sch f emptySet xs

sch f schon jetzt =
    if isEmptySet jetzt 
    then [] 
    else let alt = schon `union` jetzt
	     next = unionManySets [ f w
				  | w <- setToList jetzt ]
	     neu  = next `minusSet` alt
	 in  jetzt : sch f alt neu

