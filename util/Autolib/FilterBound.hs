-- $Header$

module FilterBound 

( filterBound )

where

-- $Log$
-- Revision 1.1  2002-05-24 10:46:48  challenger
-- Initial revision
--
-- Revision 1.1  2001/11/27 20:58:20  autotool
-- neu
--

filterBound :: Int -> (a -> Bool) -> [a] -> [a]
-- sobald ein positives Element gefunden,
-- untersuche nur noch die jeweils  b  n�chsten
filterBound b p xs =
    case dropWhile (not . p) xs of
	 [] -> []
	 x : rest -> x : restFilterBound b p rest

restFilterBound b p xs =
    let (pre, post) = splitAt b xs
    in	case filter p pre of
		 [] -> [] -- Schlu�
		 ys -> ys ++ restFilterBound b p post

