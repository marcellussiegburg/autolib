module Autolib.Set

( module Data.Set
, module Autolib.Set   
, module Autolib.Xml
)

where

--   $Id$

import Data.Set
import Data.Typeable
import Autolib.ToDoc
import Autolib.Reader

import Autolib.Util.Teilfolgen

import Autolib.Xml

instance Ord a => Container (Set a) [a] where
    label _ = "Set"
    pack = setToList
    unpack = mkSet

instance Ord a => Ord (Set a) where
    compare xs ys = compare (setToList xs) (setToList ys)

instance ( Ord a, Reader [a] ) => Reader ( Set a ) where
    readerPrec d = readerParen ( d > 9 ) $ do
        my_reserved "mkSet"
	xs <- reader
	return $ mkSet xs

instance ToDoc [a] => ToDoc (Set a)
    where toDocPrec p s = docParen (p >= fcp) 
			$ text "mkSet" <+> toDocPrec fcp (setToList s)


--  http://www.haskell.org//pipermail/haskell/2005-January/015164.html
instance ( Typeable a ) =>  Typeable ( Set a ) where
    typeOf s = mkAppTy ( mkTyCon "Set" )
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
 
 
