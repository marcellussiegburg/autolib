module Util.Zufall where

-- $Id$

import Random

entweder :: IO a -> IO a -> IO a
entweder x y = do
    f <- randomRIO (False, True)
    if f then x else y

eins :: [a] -> IO a
eins xs =  do
    k <- randomRIO (1, length xs)
    return $ xs !! (k - 1)

einige :: Int -> [a] -> IO [a]
einige n xs = sequence $ replicate n $ eins xs


repeat_until :: IO a -> (a -> Bool) -> IO a
repeat_until act p = 
    do x <- act
       -- putStr " * "
       if p x then return x else repeat_until act p

permutation :: [a] -> IO [a]
permutation xs = 
    if length xs < 2 then return xs 
    else do
	 i <- randomRIO (0, length xs - 1)
	 let (here, y : there) = splitAt i xs
	 ys <- permutation $ here ++ there
	 return $ y : ys
