module NFA.Factors where

-- $Id$

import NFA.Type
import Set
import Monad ( guard )
import FiniteMap 

import ToDoc ( ToDoc, toDoc )

-- bestimme die Sprache aller (zusammenhängenden) Teilwörter

factors :: NFAC c Int
	=> NFA c Int -> NFA c Int
factors a =
    let s = succ $ maximum $ 0 : lstates a -- neuer zustand
        f = NFA { nfa_info = funni "factors" [ info a ]
		, states = states a `union` unitSet s
		, starts = states f -- alles
		, finals = states f -- auch alles
		, trans  = plusFM_C union ( trans a ) $ collect $ do
	             ( p, c, q ) <- unCollect $ trans a
	             return ( s, c, q )
		}
    in  f
