-- $Header$

module Util.DPL 

( value, values )

where

-- dynamic programming on lists

-- $Log$
-- Revision 1.1  2002-12-17 21:26:18  joe
-- mehr fergessen
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.1  2001/11/28 07:25:23  autotool
-- neu: DPL*
--

import LazyST
import FiniteMap 

import List (inits, tails)
import Monad (guard)


type Cache s a b = STRef s (FiniteMap [a] b)

empty :: (Ord a) => ST s (Cache s a b)
empty = newSTRef emptyFM

splits :: [a] -> [([a],[a])]
splits xs = zip (inits xs) (tails xs)

dpl :: (Ord a) 
   => (a -> b) -> ([(b,b)] -> b) 
   -> Cache s a b -> [a] -> ST s b
dpl unit combine c xs = do
    f <- readSTRef c
    case lookupFM f xs of
	 Just v -> return v
	 Nothing -> do
	     v <- case xs of
		  [   ] -> error "DPL.dpl: empty list"
	          [ x ] -> return $ unit x
		  _     -> do		  
		      vpqs <- sequence $ do
			   (p, q) <- splits xs
			   guard $ not $ null p
			   guard $ not $ null q    
			   return $ do 
				  vp <- dpl unit combine c p
				  vq <- dpl unit combine c q
				  return (vp, vq)
		      return $ combine vpqs
	     f <- readSTRef c
	     writeSTRef c (addToFM f xs v)
	     return v

value :: Ord a =>  (a -> b) -> ([(b,b)] -> b) -> [a] -> b
value unit combine xs = runST ( do
    c <- empty
    dpl unit combine c xs      )

values :: Ord a => (a -> b) -> ([(b,b)] -> b) -> [[a]] -> [([a],b)]
values unit combine xss = runST ( do
    c <- empty
    vs <- mapM (dpl unit combine c) xss      
    return $ zip xss vs         )






