module Autolib.Util.Hamming where

-- | Hamming distance

hamming :: Eq a => [a] -> [a] -> Int
hamming = hammingBy (==)

hammingBy :: (a -> a -> Bool) -> [a] -> [a] -> Int
hammingBy eq xs = length . filter not . zipWith eq xs
