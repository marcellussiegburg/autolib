module Sets

( module Set
, module Sets
)

where

-- $Id$

import Set
import ToDoc
import Reader

import Util.Teilfolgen


instance Ord a => Ord (Set a) where
    compare xs ys = compare (setToList xs) (setToList ys)

instance ( Ord a, Reader a ) => Reader ( Set a ) where
    reader = do
        my_reserved "mkSet"
	xs <- listify reader
	return $ mkSet xs

instance (Ord a, Reader a) => Read (Set a) where
    readsPrec = parsec_readsPrec



instance ToDoc [a] => ToDoc (Set a)
    where toDocPrec p s = docParen (p >= fcp) 
			$ text "mkSet" <+> toDocPrec fcp (setToList s)

instance ToDoc [a] => Show (Set a)
    where show = render . toDoc


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
    
