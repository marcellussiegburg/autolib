{-# OPTIONS -fno-monomorphism-restriction #-}

-- | impedance matching:
-- provide old Data.FiniteMap interface
-- but use new Data.Map implementation

module Autolib.Data.Map where

import Data.Map
import Data.Maybe ( isJust )

type FiniteMap k a = Map k a


emptyFM = empty
isEmptyFM = Data.Map.null
sizeFM = size
unitFM = singleton
eltsFM = elems
keysFM = keys
fmToList = assocs
listToFM = fromList
lookupFM m k = Data.Map.lookup k m
lookupWithDefaultFM f a k = findWithDefault a k f
elemFM k m = isJust $ Data.Map.lookup k m
mapFM = mapWithKey
addToFM fm k a = insert k a fm
addToFM_C fun m k a = 
    insertWith fun k a m
addListToFM_C fun m pairs = 
    foldr (\ (k, a) -> insertWith fun k a) m pairs
plusFM = union
plusFM_C = unionWith
filterFM = filterWithKey
intersectFM_C = intersectionWith