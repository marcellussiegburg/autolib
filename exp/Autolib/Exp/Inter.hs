-- $Header$

module Exp.Inter

( inter
, inter_det
, inter_nondet
, std, std_sigma
, E
)

where

import NFA
import Exp
import qualified Ops
import qualified Minus
import qualified Shuffle
import qualified Quotient

import qualified Exp.Env as E

import Normalize
import Minimize

import qualified Det
import qualified Trim
import qualified Mirror

import qualified Basic

import ToDoc ( toDoc )

---------------------------------------------------------------------------

type E = E.Env (NFA Char Int)

std :: E
std =  E.make 
	  [ ("Eps", Basic.epsilon)
	  , ("Empty", Basic.empty)
	  ]

std_sigma :: String -> E
std_sigma alpha = E.plus std 
		$ E.make [ ( "Sigma", Basic.sigma alpha )
			 , ( "All"  , Basic.sigmastar alpha )
			 ]

-- All ist zwar bequem, könnte aber bei der sternhöhe
-- den falschen eindruck erzeugen:
-- sieht aus wie Sigma^* = sternhöhe eins,
-- ist aber äq. zu  complement(Empty) = sternhöhe null
-- andererseits haben wir kein komplement,
-- deswegen nehmen wir All doch mit auf.


--------------------------------------------------------------------------

-- backwards compatibility
inter = inter_det 

inter_det ::  E -> Exp -> NFA Char Int
inter_det e a = ( inter_with ( minimize . normalize ) e a )
	      { info = toDoc a }

inter_nondet ::  E -> Exp -> NFA Char Int
inter_nondet e a = ( inter_with ( normalize ) e a )
		   { info = toDoc a }


inter_with :: (forall a . Ord a => NFA Char a -> NFA Char Int) -> E -> Exp -> NFA Char Int
inter_with norm e (Ref v)    = case E.look e v of
      Just x -> x
      Nothing -> error $ "Name " ++ show v ++ " nicht gebunden"

inter_with norm e (Letter c)         = 
      norm $ Basic.letter c

inter_with norm e (Dot          l r) = 
      norm $ Ops.dot          (inter_with norm e l) (inter_with norm e r)
inter_with norm e (Left_Quotient     l r) = 
      norm $ Quotient.left_quotient(inter_with norm e l) (inter_with norm e r)
inter_with norm e (Right_Quotient     l r) = 
      norm $ Quotient.right_quotient(inter_with norm e l) (inter_with norm e r)
inter_with norm e (Union        l r) = 
      norm $ Ops.union        (inter_with norm e l) (inter_with norm e r)
inter_with norm e (Shuffle      l r) = 
      norm $ Shuffle.shuffle  (inter_with norm e l) (inter_with norm e r)
inter_with norm e (Difference   l r) = 
      norm $ Minus.minus      (inter_with norm e l) (inter_with norm e r)
inter_with norm e (SymDiff      l r) = 
      norm $ Minus.symdiff    (inter_with norm e l) (inter_with norm e r)
inter_with norm e (Intersection l r) = 
      norm $ Ops.intersection (inter_with norm e l) (inter_with norm e r)

inter_with norm e (Star x)   = norm $ Ops.star    (inter_with norm e x)
inter_with norm e (Plus x)   = norm $ Ops.plus    (inter_with norm e x)
inter_with norm e (Power p x)= norm $ Ops.power p (inter_with norm e x) 


	




