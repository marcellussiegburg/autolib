module Autolib.NFA.Mirror where

-- -- $Id$

import Autolib.NFA.Type

import Autolib.Set
import Autolib.FiniteMap
import Autolib.ToDoc

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

    

