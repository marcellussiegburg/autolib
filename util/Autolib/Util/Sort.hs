module Util.Sort

( sort, sortBy
, insert, insertBy
, nub, nubBy
, rise, riseBy
)

where

import Util.Hide

-- utils ----------------------------------------------------------

hide :: ( a -> b ) -> a -> ( b, Hide a )
hide f x = ( f x, Hide x )

seek :: ( b, Hide a) -> a
seek ( _, Hide x ) = x

sort [] = []; sort [x] = [x]
sort xs = let (pre, post) = halves xs in merge (sort pre) (sort post)
   where halves [] = ([], [])
	 halves (x : xs) = let (pre, post) = halves xs in (x : post, pre)
	 merge [] ys = ys; merge xs [] = xs
	 merge (x : xs) (y : ys) =
	       if x <= y then x : merge xs (y : ys)
			 else y : merge (x : xs) ys

sortBy :: Ord b => (a -> b) -> [a] -> [a]
sortBy f xs = map seek $ sort $ map (hide f) xs


insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y : ys) = 
    if x <= y then x : y : ys else y : insert x ys

insertBy :: Ord b => (a -> b) -> a -> [a] -> [a]
insertBy f x xs = map seek $ insert (hide f x) (map (hide f) xs)


nub :: Ord a =>  [a] -> [a]
nub [] = []
nub (x : ys) = x : nub ( filter (/= x) ys )


nubBy :: Ord b => (a -> b) -> [a] -> [a]
nubBy f xs = map seek $ nub $ map (hide f) xs

-- | produce strictly increasing subsequence
rise :: Ord a => [a] -> [a]
rise [] = []
rise (x : ys) = x : rise ( filter (> x) ys )

riseBy :: Ord b => (a -> b) -> [a] -> [a]
riseBy f xs = map seek $ rise $ map (hide f) xs
