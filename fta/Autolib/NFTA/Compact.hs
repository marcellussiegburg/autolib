module NFTA.Compact 

( compact )

where

--  $Id$

import NFTA.Type
import NFTA.Ops
import Util.Fix
import Data.FiniteMap
import qualified Relation

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
        fu = lookupWithDefaultFM cp ( error "NFTA.compact" )
    in  statemap fu a
