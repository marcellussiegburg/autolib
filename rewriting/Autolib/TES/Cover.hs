module TES.Cover where

-- $Id$

import NFTA
import TES
import SRS.Aged 

import TES.Match
import Util.Size
import Sets

import SRS.Label.Type

import SRS.Interface.Config

import Control.Monad ( guard )
import Data.FiniteMap

-- | list of ( redex, Right reducts )
--   such that redex patch is (RPO-) smaller than reduct path
-- or ( redex, Left path ) for an uncovered redex
covers :: ( TRSC v c, NFTAC (Aged c) s )
      => Label
      -> TRS v c
      -> NFTA (Aged c) s
      -> [ ( Path v c s, Either ( Path v c s ) -- not covered
			        [ Path v c s ] -- already covered
	   ) 
	 ]
covers lab trs a = do
    ( l, r ) <- rules trs
    redex @ ( p, t, fm ) <- matches a l
    let reducts = do
	    reduct @ ( p', t', fm' ) <- matches_from a r p
	    guard $ mkSet (fmToList fm') `subseteq` mkSet ( fmToList fm )
	    guard $ term_is_covered_by lab (fmap age t) (fmap age t')
	    return reduct
    return ( redex
	   , if null reducts
	     then Left ( p, term_cover lab (fmap age t) r, fm )
	     else Right reducts
	   )



