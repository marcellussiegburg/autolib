-- |  add transitions to automaton
-- possibly containing new states

module Autolib.NFA.Link where

--  $Id$

import Autolib.NFA.Type
import Autolib.ToDoc

link :: NFAC c a 
     => NFA c a 
     -> (a, c, a) 
     -> NFA c a
link a l @ (p, c, q) 
     = informed ( funni "link" [ info a, toDoc l ] )
     $ links a [ l ]	     

links :: NFAC c a 
     => NFA c a 
     -> [ (a, c, a) ]
     -> NFA c a
links a ls =
     a { nfa_info = funni "links" [ info a, toDoc ls ]
       , states = union ( states a ) $ mkSet $ do 
	    (p, c, q) <- ls
	    [p, q]
       , trans  = addListToFM_C union ( trans a ) $ do
	    (p, c, q) <- ls
	    return ( (p, c), unitSet q )
       }


