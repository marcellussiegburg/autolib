module TES.Unify where

--  $Id$

import TES.Term
import TES.Position

import Data.FiniteMap

type Substitution v c = FiniteMap v ( Term v c )

mgu :: Term v c 
    -> Term v c 
    -> Maybe ( Subsitution v c )
mgu s t = gu emptyFM (s, t)

gu :: Substitution v c
    -> ( Term v c , Term v c )
    -> Maybe ( Subsitution v c )
gu fm ( s, t ) | s == t = return fm

gu fm ( Var v , t ) = do
    guard $ not $ v `elementOf` vars t
    return $ combine fm $ listToFM [ (v, t) ]
gu fm ( s , Var v ) = gu fm v s

gu fm ( s , t ) = do BROKEN
    guard $ top s == top t
    guard $ length (children s) == length (children t)
    foldM gu fm $ zip ( children s ) ( children t ) 

