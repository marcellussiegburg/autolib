module Autolib.NFTA.Normalize where

--  $Id$

import Autolib.NFTA.Type
import Autolib.NFTA.Ops
import Autolib.FiniteMap

normalize :: ( NFTAC c s, NFTAC c Int )
	  => NFTA c s -> NFTA c Int
normalize a = 
    let fm = listToFM $ zip (lstates a) [0 .. ]
	fun = lookupWithDefaultFM fm 
	      ( error $ "Autolib.NFTA.normalize" ++ show a )
    in  statemap fun a
