module Sets

( module Set
, module Sets
)

where

-- $Id$

import Set

instance Ord a => Ord (Set a) where
    compare xs ys = compare (setToList xs) (setToList ys)

instance (Ord a, Read a) => Read (Set a) where
    readsPrec p cs = do
        ( "mkSet", cs ) <- lex cs
	( arg, cs ) <- reads cs
	return (mkSet arg, cs)

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


