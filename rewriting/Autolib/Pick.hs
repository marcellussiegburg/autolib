-- | pick some or all elements
-- ideally, complete enumeration
-- and random generation should share almost all code

module Autolib.Pick where

import Autolib.Util.Splits
import Autolib.Util.Zufall
import System.Random
import Control.Monad.State


-- | class that describes choice:
class Monad g => Pick g where
    pick :: [a] -> g a
    
-- | take all
instance Pick [] where
    pick xs = xs 

-- | take one
instance RandomGen g => Pick (State g) where
    pick xs = do
        g <- get
	let ( i, g' ) = randomR (0, pred $ length xs) g
	put g'
	return $ xs !! i

-- | take one
instance Pick IO where
    pick = eins

-------------------------------------------------------------------------

-- | a subset (in increasing order)
picks :: Pick p => Int -> [a] -> p [a]
picks 0 xs = return []
picks n (x : xs) = do
    flag <- pick $ [ True ] ++ [ False | length xs >= n ]
    if flag
       then do ys <- picks (n-1) xs ; return $ x : ys
       else do ys <- picks (n  ) xs ; return $     xs

-------------------------------------------------------------------------

-- | partitions  
-- @combine 4 = [[4], [3,1], [2,2], [2,1,1], [1,1,1,1]]@
combine i = combi i i
combi b 0 = return []
combi b k | k > 0 = do
    x <- pick [ 1 .. min k b ]
    xs <- combi x (k - x)
    return $ x : xs

-- | permutations, input has multiplicities
permute :: Pick p => [(a, Int)] -> p [a]
permute [] = return []
permute things = do
    ( pre, (x,i) : post ) <- pick $ splits things
    xs <- permute $ pre ++ [ (x, pred i) | i > 1 ] ++ post
    return $ x : xs

-- | lexicographically rising
ordered_permute :: Pick p => [(a, Int)] -> p [a]
ordered_permute things = 
    helper $ do (x,i) <- things ; return (True,x,i)
    
helper [] = return []
helper fxis = do
    ( pre, (f,x,i) : post ) <- pick 
        $ filter ( \ (pre, _) -> all (\ (f,x,i) -> not f) pre)
	$ splits fxis
    xs <- helper $ pre ++ [ (False, x, pred i) | i > 1 ] ++ post
    return $ x : xs





