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

-- | from the root to the leaves
eclosure_down :: NFTAC c s
	 => NFTA c s
	 -> s
	 -> [ s ]
eclosure_down a p = p : setToList ( Relation.images ( eps a  ) p )


-- | from the leaves to the root
eclosure_up :: NFTAC c s
	 => NFTA c s
	 -> s
	 -> [ s ]
eclosure_up a p = p : setToList ( Relation.images ( inv_eps a ) p )
