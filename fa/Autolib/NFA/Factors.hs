module NFA.Factors where

-- $Id$

import NFA.Type
import NFA.Trim

import Set

-- bestimme die Sprache aller (zusammenhängenden) Teilwörter

factors :: NFAC c a
	=> NFA c a -> NFA c a
factors a0 =
    let a = trim a0 
        -- ohne trim stimmts nicht, siehe mail von Mirko (28. 10.)
    in  a   { nfa_info = funni "factors" [ info a ]
 	    , starts = states a -- alles
	    , finals = states a -- auch alles
	    }
