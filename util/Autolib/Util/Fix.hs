module Autolib.Util.Fix where

-- -- $Id$

fixBy :: Eq b => (a -> b) -> (a -> a) -> a -> a
fixBy select f x0 = last $ fixesBy select f x0

fix :: Eq a => (a -> a) -> a -> a
fix = fixBy id

fixesBy :: Eq b => (a -> b) -> (a -> a) -> a -> [a]
fixesBy select f x0 = x0 : 
    let x1 = f x0
    in	if select x1 == select x0 then [] 
	else fixesBy select f x1

fixes :: Eq a => (a -> a) -> a -> [a]
fixes = fixesBy id