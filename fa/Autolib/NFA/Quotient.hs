module NFA.Quotient where

-- -- $Id$

import NFA.Type
import NFA.Trim
import qualified NFA.Ops
import qualified Sets

import NFA.Mirror ( mirror )

import Control.Monad ( guard )

right_quotient :: (NFAC c s, NFAC c t) => NFA c s -> NFA c t -> NFA c s
right_quotient a b =
    let c = NFA.Ops.cross a b 
	ok (p, q) =  Sets.elementOf p (finals a)	
		  && Sets.elementOf q (finals b)	
	d = productive $ c { finals = Sets.sfilter ok $ states c }

    in  a { nfa_info = funni "right_quotient" [ info a, info b ]
	  , finals = Sets.mkSet $ do
	      p <- lstates a
	      guard $ or $ do 
		    q <- lstarts b
		    return $ Sets.elementOf (p, q) (states d)
	      return p
	  }

left_quotient ::  (NFAC c s, NFAC c t) => NFA c s -> NFA c t -> NFA c s
left_quotient a b =
    informed ( funni "left_quotient" [ info a, info b ] )   
    $ mirror $ right_quotient ( mirror a ) ( mirror b )
