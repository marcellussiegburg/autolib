-- -- $Id$

module Autolib.NFA.Det 

where

import Autolib.NFA.Type
import Autolib.NFA.Trim

import Autolib.Letters
import Autolib.NFA.Hull

import Data.Set
import Autolib.Sets (sfilter)

import Control.Monad.State

import Data.FiniteMap
import Autolib.ToDoc


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
				



