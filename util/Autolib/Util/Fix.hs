module Util.Fix where

-- $Id$

fix :: Eq a => (a -> a) -> a -> a
fix f x0 = last $ fixes f x0


fixes :: Eq a => (a -> a) -> a -> [a]
fixes f x0 = x0 : 
    let x1 = f x0
    in	if x1 == x0 then [] 
	else fixes f x1
