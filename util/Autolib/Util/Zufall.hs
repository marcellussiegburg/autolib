module Autolib.Util.Zufall 

where

-- -- $Id$

import Random

entweder :: IO a -> IO a -> IO a
entweder x y = do
    f <- randomRIO (False, True)
    if f then x else y

entweders :: [ IO a ] -> IO a
entweders acts = do
    act <- eins acts
    act

eins :: [a] -> IO a
eins xs =  do
    k <- randomRIO (1, length xs)
    return $ xs !! (k - 1)

einige :: Int -> [a] -> IO [a]
einige n xs = sequence $ replicate n $ eins xs

genau :: Eq a => Int -> [a] -> IO [a]
genau 0 _ = return []
genau n xs = do x <- eins xs
		ys <- genau (pred n) $ filter (/=x) xs
		return $ x : ys

repeat_until :: IO a -> (a -> Bool) -> IO a
repeat_until act p = 
    do x <- act
       -- putStr " * "
       if p x then return x else repeat_until act p

permutation :: [a] -> IO [a]
permutation xs = selektion (length xs) xs

selektion :: Int -> [a] -> IO [a]
selektion 0 xs = return  []
selektion k xs = do
	 i <- randomRIO (0, length xs - 1)
	 let (here, y : there) = splitAt i xs
	 ys <- selektion (pred k) $ here ++ there
	 return $ y : ys


summe :: Int -> Int -> IO [ Int ]
-- finde eine zerlegung von n in genau k summanden >= 1
-- achtung: ist nicht gleichverteilt (?)
summe k n | k < 0 = error "Util.Zufall.summe: k < 0"
summe k n | n < 0 = error "Util.Zufall.summe: n < 0"
summe k n | n < k = error $ "Util.Zufall.summe: n < k" ++ show (k, n)
summe 1 n = return [n]
summe k n | n == k = return $ replicate k 1
summe k n = do    
    x <- randomRIO (1, n-k+1)
    xs <- summe (k-1) (n-x)
    return $ x : xs

