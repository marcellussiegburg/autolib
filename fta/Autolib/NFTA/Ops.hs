module NFTA.Ops where

--  $Id$

import NFTA.Type

alphamap :: ( NFTAC c s, NFTAC d s )
	 => ( c -> d )	
	 -> NFTA c s
	 -> NFTA d s
alphamap f a = 
    NFTA { states = states a
	 , finals = finals a
	 , trans = smap ( \ (ps, c, q) -> (ps, f c, q) ) 
	 	$ trans a
	 }
