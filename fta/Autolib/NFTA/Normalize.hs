module NFTA.Normalize where

--  $Id$

import NFTA
import Data.FiniteMap

normalize :: ( NFTAC c s, NFTAC c Int )
	  => NFTA c s -> NFTA c Int
normalize a = 
    let fm = listToFM $ zip (lstates a) [0 .. ]
	fun = lookupWithDefaultFM fm 
	      ( error $ "NFTA.normalize" ++ show a )
    in  statemap fun a
