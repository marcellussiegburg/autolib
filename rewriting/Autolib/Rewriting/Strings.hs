module Autolib.Rewriting.Strings where

--  $Id$

import Autolib.Rewriting.Address
import Autolib.Virtualist
import Data.List ( tails )

instance Is_Top Int where
    is_top = (== 0)

instance Sub (Virtualist v) where
    top = top . unVirtualist
    children xs = map ( virtualist ( plug xs )) $ children $ unVirtualist xs
    build x [xs] = virtualist (plug xs) $ x : unVirtualist xs
    replace xs x = virtualist (plug xs) $ replace ( unVirtualist xs ) x
    substructures xs = map (virtualist ( plug xs))
		     $ substructures $ unVirtualist xs

instance Address (Virtualist v) Int where
    addresses = addresses . unVirtualist
    peek xs k = virtualist (plug xs) $ peek ( unVirtualist xs ) k
    poke xs (k, ys) = virtualist (plug xs)
		    $ poke ( unVirtualist xs ) ( k, unVirtualist ys )
    pmap f xs = virtualist (plug xs) $ pmap f $ unVirtualist xs

instance Sub [] where
    --  top (leftmost) symbol
    -- top :: c a -> a
    top = head

    children = return . tail
    build x [ xs ] = x : xs

    --  replace top symbol
    -- replace :: c a -> a -> c a
    replace (_ : xs) x = x : xs

    -- substructures :: c a -> [ c a ]
    substructures = tails

    
instance Address [] Int where
    --  all addresses
    -- addresses :: c a -> [ p ]
    addresses xs = [ 0 .. pred $ length xs ]

    --  get substructure
    -- peek :: c a -> p -> c a
    peek xs k = drop k xs

    --  replace substructure
    -- poke :: c a -> ( p, c a ) -> c a
    poke xs ( k , ys ) = take k xs ++ ys

    --  compute new symbol at position
    -- pmap :: ( p -> a -> b ) -> c a -> c b
    pmap f xs = do
        ( pos, x ) <- zip [ 0 .. ] xs
	return $ f pos x
