--  $Id$

module Autolib.NFA.Shuffle ( shuffle ) where

import Autolib.NFA
import Autolib.NFA.Trim

import Autolib.Set hiding ( subseteq )
import Autolib.FiniteMap


shuffle :: (NFAC c s, NFAC c t) 
	=> NFA c s -> NFA c t -> NFA c (s, t)
shuffle a b = trim $ 
    NFA { nfa_info = funni "shuffle" [ info a, info b ]
	, alphabet = union ( alphabet a ) ( alphabet b )
        , states = cross (states a) (states b)
	, starts = cross (starts a) (starts b)
	, finals = cross (finals a) (finals b)
	, trans = addListToFM_C union emptyFM $ do 
	     let la = setToList $ letters a; lb = setToList $ letters b
	     pa <- lstates a; pb <- lstates b

	     do -- computation in automaton a
	          c <- la
		  qa <- setToList 
		     $ lookupWithDefaultFM (trans a) emptySet (pa, c)
		  return (((pa, pb), c), unitSet (qa, pb))
	       ++ do -- computation in automaton b
	             c <- lb
		     qb <- setToList 
			$ lookupWithDefaultFM (trans b) emptySet (pb, c)
		     return (((pa, pb), c), unitSet (pa, qb))
	}
  

