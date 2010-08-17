module Autolib.Util.Perm where

import System.Random

-- | eine zufällige Permutation
permIO :: [a] -> IO [a]
permIO [   ] = return [   ]
permIO [ x ] = return [ x ]
permIO ( x : xs ) = do
    k <- randomRIO ( 0, length xs )
    ys <- permIO xs
    let ( pre, post ) = splitAt k ys
    return $ pre ++ x : post

