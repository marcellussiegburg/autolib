-- | Quotients of languages

module Autolib.NFA.Quotient where

--  $Id$

import Autolib.NFA.Type
import Autolib.NFA.Trim
import qualified Autolib.NFA.Ops
import qualified Autolib.Set

import Autolib.NFA.Mirror ( mirror )

import Control.Monad ( guard )

-- | construction is directly on the state set of a
-- in fact we mark (as final) all states of a
-- from which a word in L(b) can be accepted
right_quotient :: (NFAC c s, NFAC c t) 
	       => NFA c s 
	       -> NFA c t 
	       -> NFA c s
right_quotient a b =
    let c = Autolib.NFA.Ops.cross a b 
	ok (p, q) =  Autolib.Set.elementOf p (finals a)	
		  && Autolib.Set.elementOf q (finals b)	
	d = productive $ c { finals = Autolib.Set.sfilter ok $ states c }

    in  a { nfa_info = funni "right_quotient" [ info a, info b ]
	  , finals = Autolib.Set.mkSet $ do
	      p <- lstates a
	      guard $ or $ do 
		    q <- lstarts b
		    return $ Autolib.Set.elementOf (p, q) (states d)
	      return p
	  }

left_quotient ::  (NFAC c s, NFAC c t) => NFA c s -> NFA c t -> NFA c s
left_quotient a b =
    informed ( funni "left_quotient" [ info a, info b ] )   
    $ mirror $ right_quotient ( mirror a ) ( mirror b )
