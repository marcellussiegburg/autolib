module Util.Mehrfache where

-- $Id$

import Data.FiniteMap

mehrfacheBy :: Ord b => (a -> b) -> [a] -> [[a]]
mehrfacheBy f xs 
    = filter ( (1 <) . length )
    $ eltsFM
    $ addListToFM_C (++) emptyFM
    $ do { x <- xs ; return ( f x, [x] ) }

