--   $Id$

module Autolib.FilterBound 

( filterBound )

where

filterBound :: Int -> (a -> Bool) -> [a] -> [a]
-- sobald ein positives Element gefunden,
-- untersuche nur noch die jeweils  b  nächsten
filterBound b p xs =
    case dropWhile (not . p) xs of
	 [] -> []
	 x : rest -> x : restFilterBound b p rest

restFilterBound b p xs =
    let (pre, post) = splitAt b xs
    in	case filter p pre of
		 [] -> [] -- Schluß
		 ys -> ys ++ restFilterBound b p post

