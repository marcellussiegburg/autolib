module NFTA.Ops where

--  $Id$

import NFTA.Type
import qualified Relation

alphamap :: ( NFTAC c s, NFTAC d s )
	 => ( c -> d )	
	 -> NFTA c s
	 -> NFTA d s
alphamap f a = 
    NFTA { states = states a
	 , finals = finals a
	 , trans = Relation.rightmap ( \ (c,qs) -> (f c, qs) )
	 	$ trans a
	 , inv_trans = Relation.leftmap ( \ (qs,c) -> (qs, f c) )
		$ inv_trans a
	 , eps   = eps a
	 , inv_eps = inv_eps a
	 }
