module NFA.Mirror where

-- $Id$

import NFA.Type

import Set
import FiniteMap
import ToDoc

mirror :: NFAC c s
       => NFA c s -> NFA c s
mirror a = 
    let trans' = collect $ do
	    (p, c, q) <- unCollect $ trans a
	    return (q, c, p)
    in	a { nfa_info = funni "mirror" [ info a ]
	  , starts = finals a
	  , finals = starts a
	  , trans = trans'
	  }

    

