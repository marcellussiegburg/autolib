module Util.Sort

where

import Util.Hide

-- utils ----------------------------------------------------------

sort [] = []; sort [x] = [x]
sort xs = let (pre, post) = halves xs in merge (sort pre) (sort post)
   where halves [] = ([], [])
	 halves (x : xs) = let (pre, post) = halves xs in (x : post, pre)
	 merge [] ys = ys; merge xs [] = xs
	 merge (x : xs) (y : ys) =
	       if x <= y then x : merge xs (y : ys)
			 else y : merge (x : xs) ys


sortBy :: Ord b => (a -> b) -> [a] -> [a]
sortBy f xs = map (unHide . snd) $ sort [ (f x, Hide x) | x <- xs ]

unique :: Ord a => [a] -> [a]
unique = nub . sort where
    nub (x : y : zs) = if x /= y then x : nub (y : zs) else nub (x : zs)
    nub xs = xs

