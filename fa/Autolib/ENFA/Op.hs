module Autolib.ENFA.Op where

--  $Id$

import Autolib.ENFA.Data
import qualified Autolib.Relation as R
import qualified Autolib.NFA as N


alphamap :: ( NFAC c a, NFAC d a ) 
	 => ( c -> d )
	 -> ENFA c a -> ENFA d a
alphamap f a =
    ENFA { enfa_info = funni "alphamap" [ info f, info a ]
	 , alphabet = smap f $ alphabet a
	 , states = states a
	 , starts = starts a
	 , finals = finals a
	 , trans = collect $ do
	      ( p, c, q ) <- unCollect $ trans a
	      return ( p, f c, q )
	 , eps  = eps a
	 }

statemap :: ( NFAC c a, NFAC c b ) 
	 => ( a -> b )
	 -> ENFA c a -> ENFA c b
statemap f a =
    ENFA { enfa_info = funni "statemap" [ info f, info a ]
	 , alphabet = alphabet a
	 , states = smap f $ states a
	 , starts = smap f $ starts a
	 , finals = smap f $ finals a
	 , trans = collect $ do
	      ( p, c, q ) <- unCollect $ trans a
	      return ( f p, c, f q )
	 , eps  = R.bothmap f f $ eps a
	 }



from_NFA :: NFAC c a => N.NFA c a -> ENFA c a
from_NFA a = 
    ENFA { enfa_info = N.nfa_info a 
	 , alphabet = N.alphabet a
	 , states = N.states a
	 , starts = N.starts a
	 , finals = N.finals a
	 , trans  = N.trans  a
	 , eps    = R.flat $ N.states a
	 }

-- | add epsilon transition
-- (resulting eps table is reflexive and transitive)
add_eps :: NFAC c a => ENFA c a -> [( a, a )] -> ENFA c a
add_eps a pqs =
    a { eps = R.reflex_trans $ R.inserts ( eps a ) pqs }

