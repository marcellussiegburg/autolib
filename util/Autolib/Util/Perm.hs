module Autolib.Util.Perm where

-- -- $Id$

import Random

permIO :: [a] -> IO [a]
-- eine zufällige Permutation
permIO [   ] = return [   ]
permIO [ x ] = return [ x ]
permIO ( x : xs ) = do
    k <- randomRIO ( 0, length xs )
    ys <- permIO xs
    let ( pre, post ) = splitAt k ys
    return $ pre ++ x : post

