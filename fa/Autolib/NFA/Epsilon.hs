module NFA.Epsilon where

-- $Id$

import NFA.Type
import Set
import Monad ( guard )
import FiniteMap 

import ToDoc ( ToDoc, toDoc )

-- füge epsilon-übergang von p nach q zu a hinzu:
-- für jeden vorgänger o -> p baue pfeil o -> q
-- falls p ein startzustand, dann q auch startzustand
-- falls q ein akz. zustand, dann p auch akz. zustand

add_epsilons :: NFAC c a
	     => NFA c a -> [(a,a)] -> NFA c a
add_epsilons a pqs
    = informed ( funni "add_epsilons" [ info a, toDoc pqs ] )
    $ foldl add_epsilon a pqs

add_epsilon :: NFAC c a
	    => NFA c a -> (a,a) -> NFA c a
add_epsilon a (p, q) = 
    NFA { nfa_info = funni "add_epsilon" [ info a, toDoc (p, q) ]
	, states = states a -- bleibt
	, starts = union ( starts a ) $ mkSet $ do
	              guard $ p `elementOf` starts a 
                      return q
	, finals = union ( finals a ) $ mkSet $ do
	              guard $ q `elementOf` finals a 
	              return p
	, trans  = plusFM_C union ( trans a ) $ collect $ do
	             ( o, c, p' ) <- unCollect $ trans a
	             guard $ p' == p
	             return ( o, c, q )
	}
