module Autolib.Util.Zufall 

( module Autolib.Util.Zufall
, module Autolib.Util.RandoM
)

where

import Autolib.Util.RandoM

-- -- $Id$

someIO :: RandomC m => [a] -> Int -> m [a]
someIO alpha 0 = return []
someIO alpha k = do
    x <- eins alpha
    xs <- someIO alpha (k-1)
    return $ x : xs

entweder :: RandomC m => m a -> m a -> m a
entweder x y = do
    f <- randomRIO (False, True)
    if f then x else y

entweders ::  RandomC m => [ m a ] -> m a
entweders acts = do
    act <- eins acts
    act

eins :: RandomC m =>  [a] -> m a
eins xs =  do
    k <- randomRIO (1, length xs)
    return $ xs !! (k - 1)

einige ::  RandomC m => Int -> [a] -> m [a]
einige n xs = sequence $ replicate n $ eins xs

genau :: ( Eq a , RandomC m ) 
      => Int -> [a] -> m [a]
genau 0 _ = return []
genau n xs = do x <- eins xs
		ys <- genau (pred n) $ filter (/=x) xs
		return $ x : ys

repeat_until ::  RandomC m => m a -> (a -> Bool) -> m a
repeat_until act p = 
    do x <- act
       -- putStr " * "
       if p x then return x else repeat_until act p

permutation ::  RandomC m => [a] -> m [a]
permutation xs = selektion (length xs) xs

selektion :: RandomC m =>  Int -> [a] -> m [a]
selektion 0 xs = return  []
selektion k xs = do
	 i <- randomRIO (0, length xs - 1)
	 let (here, y : there) = splitAt i xs
	 ys <- selektion (pred k) $ here ++ there
	 return $ y : ys


-- | finde eine zerlegung von n in genau k summanden >= 1
-- achtung: ist nicht gleichverteilt (?)
summe ::  RandomC m => Int -> Int -> m [ Int ]
summe k n | k < 0 = error "Util.Zufall.summe: k < 0"
summe k n | n < 0 = error "Util.Zufall.summe: n < 0"
summe k n | n < k = error $ "Util.Zufall.summe: n < k" ++ show (k, n)
summe 1 n = return [n]
summe k n | n == k = return $ replicate k 1
summe k n = do    
    x <- randomRIO (1, n-k+1)
    xs <- summe (k-1) (n-x)
    return $ x : xs

