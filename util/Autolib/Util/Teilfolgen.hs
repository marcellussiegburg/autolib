module Util.Teilfolgen where

-- $Id$

teilfolgen :: Int -> [a] -> [[a]]
teilfolgen k xs | k > length xs = []
teilfolgen 0 xs = [[]]
teilfolgen k (x : xs) 
    =  teilfolgen k xs
    ++ do ys <- teilfolgen (k-1) xs ; return $ x : ys
