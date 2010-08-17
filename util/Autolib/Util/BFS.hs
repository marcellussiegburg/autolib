module Autolib.Util.BFS 

( meetings
-- for testing
, generator
, duplicates
)

where

import Autolib.FiniteMap
import Data.Maybe
import Autolib.Set

-- | compute bfs ordering, emit any duplicates
-- assume that there are not many of them
-- or, we stop the program as soon as we have seen only a few
meetings :: Ord a => Int -- ^ höchstens soviele generieren
	          -> ( a -> [a] ) -> a -> [(a, a)]
meetings schranke f top = duplicates 
	       $ take schranke
	       $ generator f [ top ]

generator :: ( a -> [a] ) -> [a] -> [a]
generator f [] = []
generator f todo =
    todo ++ generator f ( do t <- todo ; f t )

duplicates :: Ord a => [a] -> [(a,a)]
duplicates = weeder emptyFM

weeder :: Ord a => FiniteMap a a -> [a] -> [(a, a)]
weeder done [] = []
weeder done ((x :: a) : xs) =
    ( do ( old :: a ) <- maybeToList $ lookupFM done x
         return ( old, x )
    ) ++ weeder ( addToFM done x x ) xs


