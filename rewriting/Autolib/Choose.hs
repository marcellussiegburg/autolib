module Autolib.Choose where

import Autolib.Pick

-- | class that describes choosables
class Choose a b where
      -- | choose elements of given size
      choose :: Pick p => a -> Int -> p b

-- | lazy infinite list
cache  :: ( Choose a b, Pick p ) => a -> [ p b ]
cache a = do s <- [ 0 .. ] ; return $ choose a s

instance ( Choose a b, Choose a c ) => Choose a (b, c) where
    choose a s = do
        sl <- pick [ 1 .. s - 1 ]
	l  <- choose a sl
	let sr = s - sl
	r  <- choose a sr
	return ( l, r )

instance Choose a b => Choose a [b] where
    choose a s | s == 0 = do
        return []
    choose a s | s  > 0 = do
	t  <- pick [ 1 .. s ] 
	x  <- choose a t 
	xs <- choose a (s - t) 
	return $ x : xs 

