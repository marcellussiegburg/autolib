-- $Header$

module Fix where

fix :: Eq a => (a -> a) -> a -> a
fix f x0 = 
    let x1 = f x0
    in	if x1 == x0 then x1 
	else fix f x1
