module Autolib.TES.Apply where

--  $Id$

import Autolib.TES.Type
import Autolib.TES.Out

import Data.FiniteMap
import Data.Maybe




-- | replace variables by variables
applyvar :: ( TRSC a c, TRSC b c )
	 => FiniteMap a b 
         -> Term a c -> Term b c
applyvar fm t = vmap 
	    ( lookupWithDefaultFM fm 
	      ( error $ "Autolib.TES.Term.applyvar" ++ show fm ++ " to " ++  show t )
	    ) t

-- | replace variables by terms
-- note the typing: subst must be total on the vars!
apply :: Ord v
      => FiniteMap v ( Term u c ) 
      -> Term v c 
      -> Term u c
apply fm ( Var v ) = lookupWithDefaultFM fm (error "Autolib.TES.Term.apply") v
apply fm ( Node c args ) = Node c $ map ( apply fm ) args


type Substitution v c = FiniteMap v ( Term v c )

-- | replace variables by terms
-- here, fm may be partial
apply_partial :: Ord v
      => Substitution v c
      -> Term v c 
      -> Term v c
apply_partial fm t @ ( Var v ) = lookupWithDefaultFM fm t v
apply_partial fm ( Node c args ) = Node c $ map ( apply_partial fm ) args

