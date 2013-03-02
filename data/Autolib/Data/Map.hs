{-# OPTIONS -fno-monomorphism-restriction #-}

-- | impedance matching:
-- provide old Data.FiniteMap interface
-- but use new Data.Map implementation

module Autolib.Data.Map 

where

import qualified Data.Map.Strict as M

type FiniteMap k a = M.Map k a


emptyFM = M.empty
isEmptyFM = M.null
sizeFM = M.size
unitFM = M.singleton
eltsFM = M.elems
keysFM = M.keys
fmToList = M.assocs
listToFM = M.fromList
delFromFM = flip M.delete
foldFM = M.foldrWithKey -- foldr or foldl ??

-- | explicit signature is necessary
-- because the new type would not use Maybe but Monad m => m instead
lookupFM :: Ord k => FiniteMap k a -> k -> Maybe a
lookupFM = flip M.lookup

lookupWithDefaultFM f a k = M.findWithDefault a k f
elemFM = M.member
mapFM = M.mapWithKey
addToFM fm k a = M.insert k a fm
addToFM_C fun m k a = M.insertWith fun k a m


addListToFM m pairs = Prelude.foldr (\ (k,a) -> M.insert k a) m pairs
addListToFM_C fun m pairs = Prelude.foldr (\ (k, a) -> M.insertWith fun k a) m pairs

-- better performance according to <http://www.haskell.org/pipermail/libraries/2005-February/003341.html>
-- BUT these are not equivalent!
-- addListToFM m = union m . fromList
-- addListToFM_C fun m = unionWith fun m . fromList

plusFM = M.union
plusFM_C = M.unionWith
filterFM = M.filterWithKey
intersectFM_C = M.intersectionWith