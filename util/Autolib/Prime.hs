module Prime where

--   $Id$

prime :: Integer -> Bool
prime n | n < 2 = False
prime n = all ( \ t -> 0 /= mod n t ) 
	      [ 2 .. truncate $ sqrt ( fromIntegral n :: Double ) ]

