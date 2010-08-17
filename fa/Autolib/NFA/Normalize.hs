module Autolib.NFA.Normalize where

import Autolib.NFA.Type

import Autolib.Set
import Autolib.FiniteMap
import Autolib.ToDoc

normalize :: NFAC c s
	  => NFA c s -> NFA c Int
normalize n = 
    let fm = listToFM $ zip (lstates n) [ 1 :: Int .. ]
	fun = lookupWithDefaultFM fm (error "Normalize")
	a = statemap fun n
    in	a { nfa_info = funni "normalize" [ info n ] }

-- GOTCHA: ohne das " :: Int " kann hugs-november-2002 
-- den record-update nicht ausführen!

