module TES.Cover where

--  $Id$

import NFTA
import TES
import SRS.Aged 

import TES.Match
import TES.Identifier
import qualified TES.Rule
import Util.Size
import Sets

import SRS.Label.Class

import SRS.Interface.Config

import Control.Monad ( guard )
import Data.FiniteMap

-- | list of ( redex, Right reducts )
-- such that redex patch is (RPO-) smaller than reduct path
-- or ( redex, Left path ) for an uncovered redex

type Covers d v s =  Config d
      -> TRS v Identifier
      -> NFTA (Aged Identifier) s
      -> [ ( Path v Identifier s, Either ( Path v Identifier s ) -- not covered
			        [ Path v Identifier s ] -- already covered
	   ) 
	 ]

{-# SPECIALIZE covers :: Covers d Identifier Int #-}

covers :: ( TRSC v Identifier, NFTAC (Aged Identifier) s )
      => Covers d v s
covers conf trs a = do
    let lab = bound_type conf
    ( l, r ) <- rules trs
    redex @ ( p, t, fm ) <- matches a l

    let reducts = do
	    reduct @ ( p', t', fm' ) <- TES.Match.from a r p
	    guard $ mkSet (fmToList fm') `subseteq` mkSet ( fmToList fm )
	    -- if clamping, then don't check heights of border rules
	    guard $ ( clamp conf && border_term conf t )
		  || term_is_covered_by lab (fmap age t) (fmap age t')
	    return reduct
    let -- conservative approach: reset the clamping symbol
        down c = if it c `elem` clamped_symbols conf
		 then aged 0 $ it c
		 else c
	-- brutal approach: reset all symbols
	full_down c = aged 0 $ it c
    return ( redex
	   , if null reducts
	     then Left ( p
		       , ( if clamp conf && border_term conf t
			   then fmap full_down else id 
			 ) $ term_cover lab (fmap age t) r
		       , fm 
		       )
	     else Right reducts
	   )

-- | does it involve border symbols?
border_term :: Config d -> Term v (Aged Identifier) -> Bool
border_term conf = any ( \ x -> it x `elem` clamped_symbols conf ) . lsyms

-- | does it involve border symbols?
border_rule :: Config d -> TES.Rule.Rule v (Aged Identifier) -> Bool
border_rule conf (l, r) = any (border_term conf) [l, r]

