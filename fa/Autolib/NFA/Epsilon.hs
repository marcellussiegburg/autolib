module Autolib.NFA.Epsilon where

-- -- $Id$

import Autolib.NFA.Type
import Autolib.Set
import Control.Monad ( guard )
import Autolib.FiniteMap 

import qualified Autolib.Relation as Relation

import Autolib.ToDoc ( ToDoc, toDoc )

-- | add one epsilon transition
add_epsilon :: NFAC c a
	    => NFA c a -> (a,a) -> NFA c a
add_epsilon a (p, q) = add_epsilons a [( p,q) ]

-- | add a list of epsilon transitions
add_epsilons :: NFAC c a
	     => NFA c a -> [(a,a)] -> NFA c a
add_epsilons a pqs =  
    let pqr = Relation.make_on ( states a, states a ) pqs
        reach = Relation.reflex_trans pqr

    in NFA { nfa_info = funni "add_epsilons" [ info a, toDoc pqs ]
        , alphabet = alphabet a
	, states = states a -- bleibt
	, starts = Relation.simages reach ( starts a ) 
	, finals = -- Relation.simages ( Relation.inverse reach ) ( finals a )
	     finals a
	, trans  = plusFM_C union ( trans a ) $ collect $ do
	             ( o, c, p ) <- unCollect $ trans a
	             q <- setToList $ Relation.images reach p
	             return ( o, c, q )
	}

