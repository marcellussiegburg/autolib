module Autolib.Util.Mehrfache where

-- -- $Id$

import Data.FiniteMap

mehrfacheBy :: Ord b => (a -> b) -> [a] -> [[a]]
-- nicht lazy genug:
{-
mehrfacheBy f xs 
    = filter ( (1 <) . length )
    $ eltsFM
    $ addListToFM_C (++) emptyFM
    $ do { x <- xs ; return ( f x, [x] ) }
-}

mehrfacheBy f xs = helper emptyFM xs where
    helper fm [] = []
    helper fm ( x : xs ) =
        let fx = f x
	    fm' = addToFM fm fx x
        in  ( case lookupFM fm (f x) of
	          Nothing -> id 
	          Just y  -> ( [ y, x ] : )
	     ) ( helper fm' xs )
