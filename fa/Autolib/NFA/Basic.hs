module Autolib.NFA.Basic where

--  $Id$

import Autolib.NFA.Type
import Autolib.NFA.Shortest (accepted)
import Data.Set
import Data.FiniteMap
import Autolib.ToDoc hiding ( empty )

-- | accepted language is empty?
is_empty :: NFAC c s => NFA c s -> Bool
is_empty a = null $ accepted a

-- | automaton that accepts empty language
empty :: NFAC c Int => NFA c Int
empty = NFA { nfa_info = text "Empty"
	    , alphabet = emptySet
	    , states = mkSet [1]
	    , starts = mkSet [1]
	    , finals = mkSet []
	    , trans = emptyFM
	    }

-- | automaton that accepts all words on alphabet
sigmastar :: NFAC c Int => [c] -> NFA c Int
sigmastar alpha =
    NFA { nfa_info = text "Sigma^*"
	, alphabet = mkSet alpha
	, states = mkSet [1]
	, starts = mkSet [1]
	, finals = mkSet [1]
	, trans = collect $ do
	      c <- alpha
	      return (1, c, 1)
	}

-- | automaton that accepts all one-letter words
sigma :: NFAC c Int => [c] -> NFA c Int
sigma alpha =
    NFA { nfa_info = funni "sigma" [ info alpha ]
	, alphabet = mkSet alpha
	, states = mkSet [0,1]
	, starts = mkSet [0]
	, finals = mkSet [1]
	, trans = collect $ do
	      c <- alpha
	      return (0, c, 1)
	}

-- | automaton that accepts just this word
word :: ( ToDoc [c], NFAC c Int ) => [c] -> NFA c Int
word w = 
    NFA { nfa_info = funni "word" [ info w ]
	, alphabet = mkSet w
	, states = mkSet [0 .. length w]
	, starts = mkSet [0]
	, finals = mkSet [length w]
	, trans = collect $ do
	      i <- [0 .. length w -1 ]
	      return (i, w!!i, i+1)
	}

-- | automaton that accepts empty word
epsilon :: ( ToDoc [c] , NFAC c Int ) => NFA c Int
epsilon = ( word [] ) 
     -- geht nicht mit hugs - Nov 2002
     -- wahrscheinlich wg. polymorph. update?
	   { nfa_info = text "Epsilon" }

-- | automaton that accepts single letter
letter :: ( ToDoc [c],  NFAC c Int ) => c -> NFA c Int
letter c = word [ c ]


