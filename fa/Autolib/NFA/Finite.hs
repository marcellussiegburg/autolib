module Autolib.NFA.Finite 

-- -- $Id$

( finite
)

where

import Autolib.NFA.Type
import qualified Autolib.Relation


finite :: NFAC c a => NFA c a -> Bool
finite a =
    let r = Relation.make $ do 
		 (p, c, q) <- unCollect $ trans a ; return (p, q) 
	t = Relation.trans r
    in  null $ filter (uncurry (==)) $ Relation.pairs t
    


