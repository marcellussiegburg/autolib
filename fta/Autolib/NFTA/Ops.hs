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

statemap :: ( NFTAC c s, NFTAC c t )
	 => ( s -> t )	
	 -> NFTA c s
	 -> NFTA c t
statemap f a = 
    NFTA { states = smap f $ states a
	 , finals = smap f $ finals a
	 , trans = Relation.bothmap f ( \ (c,qs) -> (c, map f qs))
	 	$ trans a
	 , inv_trans = Relation.bothmap ( \ (qs,c) -> (map f qs,c)) f
		$ inv_trans a
	 , eps   = Relation.bothmap f f $ eps a
	 , inv_eps = Relation.bothmap f f $ inv_eps a
	 }
