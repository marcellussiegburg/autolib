module NFTA.Epsilon 

( eclosure_down
, eclosure_up
)

where

--  $Id$

import NFTA.Type
import Sets
import qualified Relation
import qualified Relation.Type 
import Control.Monad ( guard )

{-
epsrel_down :: NFTAC c s 
       => NFTA c s
       -> Relation.Type s s
epsrel_down a = 
   ( eps a )
   { Relation.Type.source = states a 
   , Relation.Type.target = states a
   }
-}

-- | from the root to the leaves
eclosure_down :: NFTAC c s
	 => NFTA c s
	 -> s
	 -> Set s
eclosure_down a = Relation.images ( eps a )


-- | from the leaves to the root
eclosure_up :: NFTAC c s
	 => NFTA c s
	 -> s
	 -> Set s
eclosure_up a =  Relation.images ( inv_eps a )


{-
add_epsilon :: NFTAC c s
	    => NFTA c s
	    -> ( s, s ) -- ^ ( from state , to state )
	    -> NFTA c s
add_epsilon a (p, q) = 
    a { trans = trans a `union` mkSet ( do
          ( x, c, ys ) <- ltrans a
	  guard $ x == p
	  return ( q, c, ys )
	)
      }
-}
