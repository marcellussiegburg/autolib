module Autolib.Schichten

( schichten 
, schichten'
, bfs
, bfs'
, schichten_internal
)

where

import Data.Set

bfs :: Ord a => ( a -> Set a ) -> a -> [a]
bfs f x0 = bfs' f (unitSet x0)

bfs' :: Ord a => ( a -> Set a ) -> Set a -> [a]
bfs' f xs = concat $ map setToList $ schichten' f xs

schichten :: Ord a => (a -> Set a) -> a -> [ Set a ]
schichten f x0 = schichten' f (unitSet x0)

schichten' :: Ord a => (a -> Set a) -> (Set a) -> [ Set a ]
schichten' f xs = schichten_internal f emptySet xs

{-# specialize schichten  :: (Int -> Set Int) -> Int -> [ Set Int ] #-}
{-# specialize schichten' :: (Int -> Set Int) -> Set Int -> [ Set Int ] #-}

schichten_internal f schon jetzt =
    if isEmptySet jetzt 
    then [] 
    else let alt = schon `union` jetzt
	     next = unionManySets [ f w
				  | w <- setToList jetzt ]
	     neu  = next `minusSet` alt
	 in  jetzt : schichten_internal f alt neu

