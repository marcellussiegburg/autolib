{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts -fallow-undecidable-instances -fallow-incoherent-instances -fno-monomorphism-restriction #-}

module Autolib.Set

( Set
, module Autolib.Set 
, module Autolib.Xml
)

where

--   $Id$

import Data.Set 
import Data.Typeable
import Autolib.ToDoc
import Autolib.Reader

import Autolib.Xml

instance Ord a => Container (Set a) [a] where
    label _ = "Set"
    pack = toList
    unpack = fromList

instance ( Ord a, Reader [a] ) => Reader ( Set a ) where
    atomic_readerPrec d = do
        guard $ d < 9
        my_reserved "mkSet"
	xs <- reader
	return $ fromList xs

instance ToDoc [a] => ToDoc (Set a)
    where toDocPrec p s = docParen (p >= fcp) 
			$ text "mkSet" <+> toDocPrec fcp (setToList s)


instance Nice [a] => Nice (Set a)
    where nicePrec p s = docParen (p >= fcp) 
			$ text "mkSet" <+> nicePrec fcp (setToList s)


--  http://www.haskell.org//pipermail/haskell/2005-January/015164.html

instance ( Typeable a ) =>  Typeable ( Set a ) where
    typeOf s = 
        mkTyConApp
                ( mkTyCon "Set" )
		[ typeOf ((undefined :: Set a -> a) s) ]


isEmptySet = Data.Set.null

emptySet = Data.Set.empty
unitSet = Data.Set.singleton

delFromSet = flip Data.Set.delete
addToSet = flip Data.Set.insert
elementOf = Data.Set.member

cardinality = Data.Set.size

union = Data.Set.union
unionManySets = Data.Set.unions

intersect = Data.Set.intersection

minusSet = Data.Set.difference

mkSet :: Ord a => [a] -> Set a
mkSet = fromList

setToList :: Set a -> [a]
setToList = toAscList

subseteq :: Ord a => Set a -> Set a -> Bool
subseteq xs ys = Data.Set.null $ xs `difference` ys

smap :: (Ord a, Ord b) => (a -> b) -> (Set a -> Set b)
smap = Data.Set.map

sfilter :: Ord a => (a -> Bool) -> (Set a -> Set a)
sfilter = Data.Set.filter

nonempty :: Ord a => Set a -> Bool
nonempty = not . Data.Set.null 

cross :: (Ord a, Ord b) => Set a -> Set b -> Set (a, b)
cross xs ys = fromList $ do 
    x <- toList xs; y <- toList ys; return (x, y)

teilmengen :: Ord a => Int -> Set a -> [ Set a ]
teilmengen n = Prelude.map fromList . teilfolgen n . toList
    
subsets ::  Ord a => Set a -> [ Set a ]
subsets s = do n <- [ 0 .. cardinality s ] ; teilmengen n s
 
teilfolgen :: Int -> [a] -> [[a]]
teilfolgen k xs | k > length xs = []
teilfolgen 0 xs = [[]]
teilfolgen k (x : xs)
    =  teilfolgen k xs
    ++ do ys <- teilfolgen (k-1) xs ; return $ x : ys
 
