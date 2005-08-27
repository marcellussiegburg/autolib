module Autolib.ENFA.Uneps where

--  $Id$

import Autolib.ENFA.Data

import qualified Autolib.NFA as N
import Autolib.NFA.Epsilon
import qualified Autolib.Relation as R

uneps :: NFAC c s => ENFA c s -> N.NFA c s
uneps a = 
          N.NFA { N.nfa_info = enfa_info a
		, N.alphabet = alphabet a
		, N.states  = states a
		, N.starts = starts a
		, N.finals = finals a
		, N.trans = N.collect $ tunCollect $ trans a
		}
   `add_epsilons` R.pairs ( eps a )
