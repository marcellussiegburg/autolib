module Autolib.FiniteMap

( module Data.FiniteMap
, module Autolib.FiniteMap
)

where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Set

import Data.FiniteMap

-- import Text.XML.HaXml.Haskell2Xml
-- import Autolib.XmlFM
import Autolib.Xml

import Data.Typeable


instance ( Ord a, Ord b ) => Ord ( FiniteMap a b ) where
    compare f g = compare (fmToList f) (fmToList g)

instance ( Typeable a, Typeable b ) => Typeable (FiniteMap a b) where
    typeOf (_ :: FiniteMap a b) = 
	mkAppTy (mkTyCon "FiniteMap") 
	       [ typeOf (undefined :: a), typeOf (undefined :: b) ]


instance (ToDoc a, ToDoc b) => ToDoc (FiniteMap a b)
    where toDocPrec p fm = 
	      docParen (p >= fcp) $ text "listToFM" <+> toDocPrec fcp (fmToList fm)


instance ( Ord a, Reader a, Reader b ) => Reader ( FiniteMap a b ) where
    reader = do
        my_reserved "listToFM"
	xys <-  reader
	return $ listToFM xys


instance (Ord a ) => Container (FiniteMap a b) [(a, b)] where
    label _ = "FiniteMap"
    pack = fmToList
    unpack = listToFM


mergeFM :: (Ord a, Ord b) => 
        FiniteMap a (Set b) -> FiniteMap a (Set b) -> FiniteMap a (Set b)
mergeFM l r = plusFM_C union l r

{-# INLINE lookupset #-}
lookupset :: Ord a => FiniteMap a (Set b) -> a -> Set b
lookupset fm x = case lookupFM fm x of
    Just m -> m; Nothing -> emptySet

