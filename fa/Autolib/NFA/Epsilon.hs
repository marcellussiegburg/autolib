module Autolib.NFA.Epsilon where

-- -- $Id$

import Autolib.NFA.Type
import Data.Set
import Control.Monad ( guard )
import Data.FiniteMap 

import qualified Autolib.Relation as Relation

import Autolib.ToDoc ( ToDoc, toDoc )


add_epsilon :: NFAC c a
	    => NFA c a -> (a,a) -> NFA c a
add_epsilon a (p, q) = add_epsilons a [( p,q) ]


add_epsilons :: NFAC c a
	     => NFA c a -> [(a,a)] -> NFA c a
add_epsilons a pqs =  
    let eps = Relation.plus ( Relation.make pqs ) ( Relation.flat $ states a )
        reach = Relation.trans eps

    in NFA { nfa_info = funni "add_epsilons" [ info a, toDoc pqs ]
	, states = states a -- bleibt
	, starts = Relation.simages reach ( starts a ) 
	, finals = Relation.simages ( Relation.inverse reach ) ( finals a )
	, trans  = plusFM_C union ( trans a ) $ collect $ do
	             ( o, c, p ) <- unCollect $ trans a
	             q <- setToList $ Relation.images reach p
	             return ( o, c, q )
	}

