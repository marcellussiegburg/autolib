module Autolib.Sets

( module Autolib.Data.Set
, module Autolib.Sets
)

where

--   $Id$

import Data.Set
import Data.FiniteMap
import Autolib.ToDoc
import Autolib.Reader

import Autolib.Util.Teilfolgen

import Text.XML.HaXml.Haskell2Xml
import Autolib.XmlSet

instance Ord a => Ord (Set a) where
    compare xs ys = compare (setToList xs) (setToList ys)

instance ( Ord a, Reader [a] ) => Reader ( Set a ) where
    readerPrec d = readerParen ( d > 9 ) $ do
        my_reserved "mkSet"
	xs <- reader
	return $ mkSet xs

instance (Ord a, Reader [a]) => Read (Set a) where
    readsPrec = parsec_readsPrec

instance ToDoc [a] => ToDoc (Set a)
    where toDocPrec p s = docParen (p >= fcp) 
			$ text "mkSet" <+> toDocPrec fcp (setToList s)

instance ToDoc [a] => Show (Set a)
    where show = render . toDoc


instance (Ord a, Haskell2Xml a) => Haskell2Xml (Set a) where
    toContents s = toContents $ XmlSet $ setToList s 
    fromContents cs = 
        let ( XmlSet x, rest ) = fromContents cs
	in  ( mkSet x, rest )
    toHType (_ :: Set a) = toHType (undefined :: XmlSet a) -- ??

subseteq :: Ord a => Set a -> Set a -> Bool
subseteq xs ys = isEmptySet $ xs `minusSet` ys


smap :: (Ord a, Ord b) => (a -> b) -> (Set a -> Set b)
smap f = mkSet . map f . setToList

sfilter :: Ord a => (a -> Bool) -> (Set a -> Set a)
sfilter p = mkSet . filter p . setToList

nonempty :: Ord a => Set a -> Bool
nonempty s = not (isEmptySet s)

cross :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
cross xs ys = mkSet $ do x <- setToList xs; y <- setToList ys; return (x, y)

teilmengen :: Ord a => Int -> Set a -> [ Set a ]
teilmengen n = map mkSet . teilfolgen n . setToList
    
subsets ::  Ord a => Set a -> [ Set a ]
subsets s = do n <- [ 0 .. cardinality s ] ; teilmengen n s
 
{-# INLINE lookupset #-}
lookupset :: Ord a => FiniteMap a (Set b) -> a -> Set b
lookupset fm x = case lookupFM fm x of
    Just m -> m; Nothing -> emptySet
 
