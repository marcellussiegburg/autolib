module Util.Wort where

-- $Id$

import Util.Sort
import Util.Uniq

import Set
import Random

import Data.List ( inits, tails )

zerlegungen :: [a] -> [([a], [a])]
zerlegungen [] = [ ([], []) ]
zerlegungen (x : xs) = ([], x : xs) :
    do (vorn, hinten) <- zerlegungen xs
       return (x : vorn, hinten)

alle :: [a] -> Int -> [[a]]
alle sigma 0 = [[]]
alle sigma n = do
    w <- alle sigma (n-1)
    x <- sigma
    return $ x : w

alles :: [a] -> Int -> [[a]]
alles sigma n = do s <- [0 .. n]; alle sigma s

factors :: [a] -> [[a]]
factors w = do
    u <- tails w
    inits u

someIO :: [a] -> Int -> IO [a]
someIO sigma 0 = return []
someIO sigma n = do
    w <- someIO sigma (n-1)
    k <- randomRIO (0, length sigma-1)
    let x = sigma !! k
    return $ x : w

shuffle :: Ord a => [a] -> [a] -> [[a]]
shuffle xs [] = [ xs ]
shuffle [] ys = [ ys ]
shuffle ( x : xs ) ( y : ys ) = uniq $
     do w <- shuffle xs (y : ys); return $ x : w
  ++ do w <- shuffle (x : xs) ys; return $ y : w

hint :: Show [a] => [ a ] -> String
hint xs = 
    let 
	(erst, dann) = splitAt 5 xs
    in	show erst
	++ if null dann then ""
	   else "... noch " ++ show (length dann) ++ " Stück"

