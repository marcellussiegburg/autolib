module Autolib.NFTA.Compact 

( compact )

where

--  $Id$

import Autolib.NFTA.Type
import Autolib.NFTA.Ops
import Autolib.Util.Fix
import Autolib.FiniteMap
import qualified Autolib.Relation as Relation

-- | collect equivalent states into one

compact :: NFTAC c s 
	=> NFTA c s 
	-> NFTA c s
compact = fix step

step :: NFTAC c s 
	=> NFTA c s 
	-> NFTA c s
step a = 
    let fm = addListToFM_C (++) emptyFM $ do
	       q <- lstates a
	       return ( Relation.images ( trans a ) q , return q )
	cp = listToFM $ do
	       ( _ , qs ) <- fmToList fm
	       q <- qs
	       return ( q, head qs )
        fu = lookupWithDefaultFM cp ( error "Autolib.NFTA.compact" )
    in  statemap fu a
