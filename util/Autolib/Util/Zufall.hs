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


repeat_until :: IO a -> (a -> Bool) -> IO a
repeat_until act p = 
    do x <- act
       -- putStr " * "
       if p x then return x else repeat_until act p