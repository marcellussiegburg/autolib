module NFA.Basic where

-- -- $Id$

import NFA.Type
import Data.Set
import Data.FiniteMap
import ToDoc hiding ( empty )

empty :: NFAC c Int => NFA c Int
empty = NFA { nfa_info = text "Empty"
	    , states = mkSet [1]
	    , starts = mkSet [1]
	    , finals = mkSet []
	    , trans = emptyFM
	    }

sigmastar :: NFAC c Int => [c] -> NFA c Int
sigmastar alpha =
    NFA { nfa_info = text "Sigma^*"
	, states = mkSet [1]
	, starts = mkSet [1]
	, finals = mkSet [1]
	, trans = listToFM $ do
	      c <- alpha
	      return ((1, c), unitSet 1)
	}

sigma :: NFAC c Int => [c] -> NFA c Int
sigma alpha =
    NFA { nfa_info = text "Sigma"
	, states = mkSet [0,1]
	, starts = mkSet [0]
	, finals = mkSet [1]
	, trans = listToFM $ do
	      c <- alpha
	      return ((0, c), unitSet 1)
	}

word :: ( ToDoc [c], NFAC c Int ) => [c] -> NFA c Int
word w = 
    NFA { nfa_info = toDoc w
	, states = mkSet [0 .. length w]
	, starts = mkSet [0]
	, finals = mkSet [length w]
	, trans = listToFM $ do
	      i <- [0 .. length w -1 ]
	      return ((i, (w!!i)), unitSet (i+1))
	}

epsilon :: ( ToDoc [c] , NFAC c Int ) => NFA c Int
epsilon = ( word [] ) 
     -- geht nicht mit hugs - Nov 2002
     -- wahrscheinlich wg. polymorph. update?
	   { nfa_info = text "Epsilon" }

letter :: ( ToDoc [c],  NFAC c Int ) => c -> NFA c Int
letter c = word [ c ]


