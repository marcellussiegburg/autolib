-- -- $Id$

module NFA.Det 

where

import NFA.Type
import NFA.Trim

import Letters
import NFA.Hull

import Data.Set
import Sets (sfilter)

import Control.Monad.State

import Data.FiniteMap
import ToDoc


dtrans :: (NFAC c s )
       => [ c ] -> Set s -> FiniteMap (s, c) (Set s)
       -> FiniteMap (Set s, c) (Set s)
dtrans alpha starts tab = 
    let next xs c = unionManySets $ do
		x <- setToList xs
		return $ lookupWithDefaultFM tab emptySet (x, c)
    in	listToFM $ hull alpha next starts
    

det, det0 :: ( NFAC c s, NFAC c (Set s) ) => NFA c s -> NFA c (Set s)
det = trim . det0

det0 a = 
    let alpha = setToList $ letters a
	trans' = dtrans alpha (starts a) (trans a)
	states' = mkSet $ starts a : ( map fst $ keysFM trans' )
    in	
	NFA { nfa_info = funni "det" [ info a ]
	    , states = states'
	    , starts = unitSet $ starts a
	    , trans = mapFM ( \ k xs -> unitSet xs ) trans'
	    , finals = sfilter ( \ xs ->  
		      not $ isEmptySet $ intersect xs (finals a) ) states'
	    }

-------------------------------------------------------------------------
				



