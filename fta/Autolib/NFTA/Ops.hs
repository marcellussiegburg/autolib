module Autolib.NFTA.Ops where

--  $Id$

import Autolib.NFTA.Type
import qualified Autolib.Relation as Relation

alphamap :: ( NFTAC c s, NFTAC d s )
	 => ( c -> d )	
	 -> NFTA c s
	 -> NFTA d s
alphamap f a = 
    NFTA { states = states a
	 , finals = finals a
	 , trans = Relation.rightmap ( \ (c,qs) -> (f c, qs) )
	 	$ trans a
	 , eps   = eps a
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
	 , eps   = Relation.bothmap f f $ eps a
	 }
