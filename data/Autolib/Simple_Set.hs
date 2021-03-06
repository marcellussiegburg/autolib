--   $Id$

module Autolib.Simple_Set

-- simple Implementierung,
-- die möglichst lazy ist

( Set, -- abstrakt exportiert
        emptySet,     -- :: Set a
	mkSet,        -- :: Ord a => [a]  -> Set a
	setToList,    -- :: Set a -> [a] 
	unitSet,      -- :: a -> Set a
	singletonSet, -- :: a -> Set a

	union,          -- :: Ord a => Set a -> Set a -> Set a
	unionManySets,  -- :: Ord a => [Set a] -> Set a
	minusSet,       -- :: Ord a => Set a -> Set a -> Set a
	mapSet,         -- :: Ord a => (b -> a) -> Set b -> Set a
	intersect,      -- :: Ord a => Set a -> Set a -> Set a

	elementOf,      -- :: Ord a => a -> Set a -> Bool
	isEmptySet,     -- :: Set a -> Bool
	
	cardinality     -- :: Set a -> Int
)

where

import Data.List ( nub, (\\) )

data Set a = Set { contents :: [ a ] }
     deriving (Eq)


emptySet :: Set a
emptySet = Set []

mkSet :: Ord a => [a]  -> Set a
mkSet = Set . nub

setToList :: Set a -> [a] 
setToList = contents

unitSet :: a -> Set a
unitSet x = Set [ x ]

singletonSet :: a -> Set a
singletonSet = unitSet

union :: Ord a => Set a -> Set a -> Set a
union xs ys = mkSet ( contents xs ++ contents ys )

unionManySets :: Ord a => [Set a] -> Set a
unionManySets = mkSet . concat . map contents

minusSet :: Ord a => Set a -> Set a -> Set a
minusSet xs ys = mkSet ( contents xs \\ contents ys )

mapSet :: Ord a => (b -> a) -> Set b -> Set a
mapSet f = mkSet . map f . contents

intersect :: Ord a => Set a -> Set a -> Set a
intersect xs ys = mkSet ( filter (`elem` contents ys) $ contents xs )

elementOf :: Ord a => a -> Set a -> Bool
elementOf x xs = x `elem` contents xs

isEmptySet :: Set a -> Bool
isEmptySet = null . contents
	
cardinality :: Set a -> Int
cardinality = length . contents

