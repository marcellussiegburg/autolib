--   $Id$

module Autolib.Fix where

{-# inline fix #-}

fix :: Eq a => (a -> a) -> a -> a
fix f x0 = 
    let x1 = f x0
    in	if x1 == x0 then x1 
	else fix f x1

mfix :: ( Monad m, Eq c )
     => ( c -> m c ) 
     -> c
     -> m c
mfix f x = do
    y <- f x
    if y == x then return x
	      else mfix f y
