-- | term unification
-- using straightforward algorithm (not efficient)
--  $Id$

module TES.Unify 

( mgu
)

where

import TES.Term
import TES.Position

import Data.FiniteMap
import Data.Set
import Control.Monad

-- | find most general unifier
mgu :: TRSC v c
    => Term v c 
    -> Term v c 
    -> Maybe ( Substitution v c )
mgu s t | s == t = return $ emptyFM

mgu ( Var v ) t = do
    guard $ not $ v `elementOf` vars t
    return $ listToFM [ (v, t) ]
mgu s ( Var v ) = mgu ( Var v ) s

mgu s  t = do 
    guard $ top s == top t
    mgus ( children s ) ( children t )

mgus :: TRSC v c
     => [ Term v c ]
     -> [ Term v c ]
     -> Maybe ( Substitution v c )
mgus [] [] = return emptyFM
mgus (x : xs) (y : ys) = do
    u <- mgu x y
    let xs' = map (apply_partial u) xs
	ys' = map (apply_partial u) ys
    us <- mgus xs' ys'
    return $ plusFM_C (error "TES.Unify.mgus: clash") u us
mgus _ _ = mzero -- different lengths, doesn't unify


