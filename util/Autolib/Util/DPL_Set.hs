-- -- $Id$

module Autolib.Util.DPL_Set

( value, values )

where


import qualified Autolib.Util.DPL as D
import Autolib.Simple_Set
import Control.Monad ( guard )
-- import Data.Set

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

