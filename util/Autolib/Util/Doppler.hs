module Autolib.Util.Doppler 

( doppler
)

where

-- -- $Id$

-- do BFS, emit duplicate pairs
-- i. e. identical items with different paths

import Autolib.FiniteMap

doppler :: Ord a 
	=> Int
	-> (a -> [a]) 
        -> a 
	-> [(a,a)]
doppler schranke f x = action $ start schranke f x

action :: Ord a => State a -> [(a, a)]

action s | sizeFM ( seen s ) > schrank s = []

action s = case current s of
    [] -> case future s of
	       [] -> []
	       f -> action $ s { current = f
			       , future = [] 
			       }
    ( x : xs ) -> 
	case lookupFM ( seen s ) x of
	     Nothing -> action $ s  { seen = addToFM ( seen s ) x x
				    , current = xs 
				    , future = next s x ++ future s
				    }
	     Just y  -> ( y, x ) : action (  s { current = xs } )

data State a = 
     State { schrank :: Int
	   , next :: a -> [ a ]
	   , seen :: FiniteMap a a
	   , current :: [ a ] 
	   , future :: [ a ]
	   }

start :: Int ->  (a -> [a]) -> a -> State a
start s f x = State { schrank = s
		, next = f
		, seen = emptyFM
		, current = [x]
		, future = []
		}


