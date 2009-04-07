-- | genetic operators for lists

module Autolib.Genetic.Operator where

import Autolib.Util.Zufall
import Control.Monad ( forM )

-- * useful

-- | some splitting. 
-- if slice xs ==> (u,v,w) then u ++ v ++ w == xs
slice :: [a] -> IO ([a], [a], [a])
slice xs = do
    m <- randomRIO ( 1, length xs - 1 )
    s <- randomRIO ( 0, length xs - m )
    let ( pre, midpost ) = splitAt s xs
        ( mid, post ) = splitAt m midpost
    return ( pre, mid, post )

-- * mutations

-- | remove some contiguous subsequence
cut :: [a] -> IO [a]
cut xs = do
    ( pre, mid, post ) <- slice xs
    return $ pre ++ post
                      
-- * numerical
    
-- | change the middle piece of a three-way split
-- uniformly by +1 or by -1
shift :: [Int] -> IO [Int]
shift xs = do
    ( pre, mid, post ) <- slice xs
    off <- eins [ -1, 1 ]
    return $ pre ++ map (off +) mid ++ post

-- | to each element, with probability 1/length xs, 
-- add or subtract one
uniform xs = forM xs $ \ x -> do
    act <- randomRIO ( 0, length xs )
    if act == 0
       then eins [ x-1, x+1 ] 
       else return x
    
-- * combinations
-- | onepoint crossover, varying result length
onepoint_varying xs ys = do
    i <- randomRIO ( 1, length xs ) 
    j <- randomRIO ( 0, length ys - 1 )
    return $ take i xs ++ drop j ys

-- | twopoint crossover, varying result length
twopoint_varying xs ys = do
    ( pre, _, post ) <- slice xs
    ( _, mid, _ ) <- slice ys
    return $ pre ++ mid ++ post

-- | onepoint crossover, both args and result have equal lengths
onepoint xs ys = do
    i <- randomRIO ( 0, length xs ) 
    return $ take i xs ++ drop i ys

-- | twopoint crossover, both args and result have equal lengths
twopoint xs ys = do
    ( pre, mid, post ) <- slice xs
    let mid' = take ( length mid ) $ drop ( length pre ) ys
    return $ pre ++ mid' ++ post

-- | for each point independently, decide about parent.
-- args and result have equal lengths
eachpoint xs ys = forM ( zip xs ys ) $ \ (x,y) -> eins [x,y]

-- | mixture, varying result length
zipper [] ys = return ys
zipper xs [] = return xs
zipper (x:xs) ys = do
    flip <- randomRIO ( False, True )
    let here = [ x | flip ]
    rest <- zipper ys xs
    return $ here ++ rest
