module Autolib.Util.Doppelte

-- -- $Id$

( doppelte
, doppelteBy
)

where

import Autolib.Util.Hide
import Autolib.FiniteMap



doppelteBy :: Ord b => (a -> b) -> [a] -> [(a,a)]
doppelteBy f 
    = map ( \ (x,y) -> let unbox = unHide . snd in (unbox x, unbox y) )
    . doppelte  
    . map ( \ x -> (f x, Hide x) )

doppelte :: Ord a => [a] -> [(a,a)]
doppelte = suche emptyFM 

suche :: Ord a => FiniteMap a a -> [a] -> [(a,a)]
suche schon [] = []
suche schon (x : xs) =
    case lookupFM schon x of
        Just y -> (x, y) : suche schon xs
	Nothing -> suche (addToFM schon x x) xs
