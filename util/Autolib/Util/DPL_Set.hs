-- $Header$

module Util.DPL_Set

( value, values )

where

-- $Log$
-- Revision 1.1  2002-12-17 21:24:26  joe
-- fergessen
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.2  2001/11/29 13:41:20  autotool
-- CF_advanced: besserer parser, härtere tests
--
-- Revision 1.1  2001/11/28 07:25:23  autotool
-- neu: DPL*
--

import qualified Util.DPL as D
import Simple_Set
import Monad ( guard )
-- import Set

lift ::  Ord b
     => ( (b,b) -> Set b )  
     -> ( [(Set b, Set b)] -> Set b )
lift combine xys = mkSet $ do
     (xs, ys) <- xys
     guard $ not $ isEmptySet xs
     guard $ not $ isEmptySet ys 
     x <- setToList xs
     y <- setToList ys
     setToList $ combine (x, y)

value :: (Ord a, Ord b) 
      =>  (a -> Set b) -> ((b,b) -> Set b) -> [a] -> Set b
value unit combine = D.value unit (lift combine)

values :: (Ord a, Ord b) 
      =>  (a -> Set b) -> ((b,b) -> Set b) -> [[a]] -> [([a],Set b)]
values unit combine = D.values unit (lift combine)

