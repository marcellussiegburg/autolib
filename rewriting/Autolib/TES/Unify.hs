-- | term unification
-- using straightforward algorithm (not efficient)
--  $Id$

module TES.Unify 

( mgu
, match
)

where

import TES.Term
import TES.Symbol
import TES.Position

import Data.FiniteMap
import Data.Maybe
import Data.Set
import Control.Monad

import ToDoc

-- | view variables as symbols
pack :: Term v c -> Term any (Either v c)
pack ( Var v ) = Node ( Left v ) []
pack ( Node f args ) = Node ( Right f ) ( map pack args )

unpack :: Term any (Either v c) -> Term v c
unpack ( Node ( Left v ) [] ) = Var v
unpack ( Node ( Right f ) args ) = Node f ( map unpack args )

-- | will only bind variables in the left side
match :: ( Ord v, Ord w, Eq c )
      => Term v c
      -> Term w c
      -> Maybe ( FiniteMap v ( Term w c ) )
match l r = do
    u <- mgu ( fmap Right l ) ( pack r )
    return $ mapFM ( \ v t -> unpack t ) u

-- | find most general unifier
mgu :: ( Ord v , Eq c )
    => Term v c 
    -> Term v c 
    -> Maybe ( Substitution v c )
mgu s t | s == t = return $ emptyFM

mgu ( Var v ) t = do
    guard $ not $ v `elementOf` vars t
    let fm = listToFM [ (v, t) ]
    return fm
mgu s ( Var v ) = mgu ( Var v ) s

mgu s  t = do 
    guard $ top s == top t
    mgus ( children s ) ( children t )

mgus :: ( Ord v, Eq c )
     => [ Term v c ]
     -> [ Term v c ]
     -> Maybe ( Substitution v c )
mgus [] [] = return emptyFM
mgus xxs @ (x : xs) yys @ (y : ys) = do
    u <- mgu x y
    let fu = apply_partial u
    let xs' = map fu xs
	ys' = map fu ys
    us <- mgus xs' ys'
    let u' = mapFM ( \ v t -> apply_partial us t ) u
    return $ plusFM_C (error $ "TES.Unify.mgus: clash" ) u' us
mgus _ _ = mzero -- different lengths, doesn't unify


---------------------------------------------------------

check l r = do
    ( p, s ) <- positions r
    u <- maybeToList $ match l s
    return (p, s, u)

l =  Node 'f' [ Var 0 ] 
r = Node 'h' [ Node 'f' [ Node 'g' [ Var 0 ] ] ]
