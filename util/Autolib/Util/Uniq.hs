module Util.Uniq where

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x : xs) = x : filter ( /= x ) ( uniq xs )
