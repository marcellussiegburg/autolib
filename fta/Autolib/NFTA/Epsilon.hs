module Autolib.NFTA.Epsilon 

( eclosure_down
, eclosure_up
, uneps
)

where

--  $Id$

import Autolib.NFTA.Type
import Autolib.Sets
import qualified Autolib.Relation as Relation
-- import qualified Autolib.Relation.Type 
import Control.Monad ( guard )
import Autolib.Util.Uniq

-- | from the root to the leaves
eclosure_down :: NFTAC c s
	 => NFTA c s
	 -> s
	 -> [ s ]
eclosure_down a p = 
    p : filter (/= p) ( setToList $ Relation.images ( eps a  ) p )


-- | from the leaves to the root
eclosure_up :: NFTAC c s
	 => NFTA c s
	 -> s
	 -> [ s ]
eclosure_up a p = 
    p : filter (/= p) 
       ( setToList $ Relation.images ( Relation.inverse $ eps a ) p )


-- | find equivalent automaton without epsilon transitions
uneps :: NFTAC c s
      => NFTA c s
      -> NFTA c s
uneps a = a 
	{ finals    = Relation.simages ( self_eps a ) ( finals a )
	, trans     = Relation.times  ( self_eps a  ) ( trans a    )
	, eps       = Relation.empty ( states a )
	}
