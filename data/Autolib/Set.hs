{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts -fallow-undecidable-instances #-}

module Autolib.Set

( module Data.Set
, module Autolib.Set   
, module Autolib.Xml
)

where

--   $Id$

import Data.Set 
#if (__GLASGOW_HASKELL__ >= 604)
       hiding ( filter, map, null, size, split
	      , empty, intersection, insert, valid, singleton
	      , (\\), fold, partition, delete
	      )
#endif
import Data.Typeable
import Autolib.ToDoc
import Autolib.Reader

import Autolib.Util.Teilfolgen

import Autolib.Xml

instance Ord a => Container (Set a) [a] where
    label _ = "Set"
    pack = setToList
    unpack = mkSet

#if (__GLASGOW_HASKELL__ < 604)
instance Ord a => Ord (Set a) where
    compare xs ys = compare (setToList xs) (setToList ys)
#endif

instance ( Ord a, Reader [a] ) => Reader ( Set a ) where
    atomic_readerPrec d = do
        guard $ d < 9
        my_reserved "mkSet"
	xs <- reader
	return $ mkSet xs

instance ToDoc [a] => ToDoc (Set a)
    where toDocPrec p s = docParen (p >= fcp) 
			$ text "mkSet" <+> toDocPrec fcp (setToList s)


--  http://www.haskell.org//pipermail/haskell/2005-January/015164.html

instance ( Typeable a ) =>  Typeable ( Set a ) where
    typeOf s = 
#if (__GLASGOW_HASKELL__ < 604)
        mkAppTy 
#else
        mkTyConApp
#endif
                ( mkTyCon "Set" )
		[ typeOf ((undefined :: Set a -> a) s) ]


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
 
 
