module TES.Cover where

-- $Id$

import NFTA
import TES
import SRS.Aged 

import TES.Match
import Util.Size
import Sets

import Control.Monad ( guard )
import Data.FiniteMap

-- | list of ( redex, Right reducts )
--   such that redex patch is (RPO-) smaller than reduct path
-- or ( redex, Left path ) for an uncovered redex
covers :: ( TRSC v c, NFTAC (Aged c) s )
      => TRS v c
      -> NFTA (Aged c) s
      -> [ ( Path v c s, Either ( Path v c s ) -- not covered
			        [ Path v c s ] -- already covered
	   ) 
	 ]
covers trs a = do
    ( l, r ) <- rules trs
    redex @ ( p, t, fm ) <- matches a l
    let reducts = do
	    reduct @ ( p', t', fm' ) <- matches_from a r p
	    guard $ mkSet (fmToList fm') `subseteq` mkSet ( fmToList fm )
	    guard $ is_covered_by t t'
	    return reduct
    return ( redex
	   , if null reducts
	     then Left ( p, cover t r, fm )
	     else Right reducts
	   )

-- | construct labelled rhs
-- use standard match bounds ( 1 + minimum )
cover :: Ord c 
      => Term s ( Aged c ) -- ^ labelled lhs
      -> Term v c -- ^ un-labelled rhs
      -> Term v ( Aged c )
cover lhs r = 
    let m = succ $ mini lhs
    in	fmap ( Aged m ) r

is_covered_by :: Ord c 
	      => Term a (Aged c) 
	      -> Term b (Aged c) 
	      -> Bool
is_covered_by  t t' =
     size t' == 0 || mini t < mini t'

mini :: Ord c 
     => Term v (Aged c) -> Int
mini t = minimum $ map age $ setToList $ syms t



