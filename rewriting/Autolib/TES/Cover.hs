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
import Data.List ( nub )

-- | list of ( redex, Right reducts )
-- such that redex patch is (RPO-) smaller than reduct path
-- or ( redex, Left path ) for an uncovered redex

type Covers d v s =  Config d
      -> TRS v Identifier
      -> NFTA (Aged Identifier) s
      -> [ ( Path v Identifier s
	   , Either ( Path v Identifier s ) -- not covered
		    [ ( Bool, Path v Identifier s) ] -- already covered
	   ) 
	 ]

{-# SPECIALIZE covers :: Covers d Identifier Int #-}

{- TODO
relative_covers conf a =
     let conf' = if   clamp conf
                   then conf { bound_type = Label Zero }
                   else conf
       in    covers conf  ( base_srs  conf ) a
          ++ covers conf' ( extra_srs conf ) a
-}

covers :: ( TRSC v Identifier, NFTAC (Aged Identifier) s )
      => Covers d v s
covers conf trs a = do
    let lab = bound_type conf
    ( l, r ) <- rules trs
    redex @ ( p, t, fm ) <- matches a l
    let fat = fmap age t 

    let ( clipper , unclipper ) = case clip conf of
		Nothing -> ( id, id )
		Just c  -> ( fmap ( agemap $ min c ) 
			   , fmap ( agemap $ \ h -> if h >= c then h+1 else h )
			   )

    let reducts = do 
	    reduct @ ( p', t', fm' ) <- TES.Match.from a r p
	    guard $ mkSet (fmToList fm') `subseteq` mkSet ( fmToList fm )
	    let ct' = unclipper t'
            let ( ex, cex ) = ( term_is_covered_by lab fat $ fmap age t'
			      , term_is_covered_by lab fat $ fmap age ct'
			      )
            guard $ ex || cex
	    -- first item is False iff exact cover, True iff clipped
	    return ( not ex, reduct ) 
    return ( redex
	   , if null reducts
	     then Left ( p
		       , clipper $ term_cover lab fat r
		       , fm 
		       )
	     else Right reducts
	   )



