module Rewriting.Strings where

--  $Id$

import Rewriting.Address


instance Address [] Int where
    -- | top (leftmost) symbol
    -- top :: c a -> a
    top = head

    -- | replace top symbol
    -- replace :: c a -> a -> c a
    replace (_ : xs) x = x : xs

    -- | all addresses
    -- addresses :: c a -> [ p ]
    addresses xs = [ 0 .. pred $ length xs ]

    -- | get substructure
    -- peek :: c a -> p -> c a
    peek xs k = drop k xs

    -- | replace substructure
    -- poke :: c a -> ( p, c a ) -> c a
    poke xs ( k , ys ) = take k xs ++ ys
