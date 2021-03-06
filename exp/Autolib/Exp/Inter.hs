module Autolib.Exp.Inter

( inter
, inter_det
, inter_nondet
, std, std_sigma
, E
)

where

import Autolib.NFA hiding ( Dot )

import Autolib.Exp.Type
import Autolib.Exp.Read
import Autolib.Exp.Print

import qualified Autolib.NFA.Ops      as Ops
import qualified Autolib.NFA.Minus    as Minus
import qualified Autolib.NFA.Shuffle  as Shuffle
import qualified Autolib.NFA.Quotient as Quotient

import qualified Autolib.Exp.Env as E

import qualified Autolib.NFA.Det      as Det
import qualified Autolib.NFA.Trim     as Trim
import qualified Autolib.NFA.Mirror   as Mirror
import qualified Autolib.NFA.Basic    as Basic

import Autolib.Symbol
import Autolib.ToDoc
import Autolib.Reader

---------------------------------------------------------------------------

type E c = E.Env (NFA c Int)


-- | standard environment (binds Eps and Empty)
std :: NFAC c Int
    => E c
std =  E.make 
	  [ ("Eps", Basic.epsilon)
	  , ("Empty", Basic.empty)
	  ]

-- | standard environment for specified alphabet
-- binds Eps, Empty, Sigma, All

-- All ist zwar bequem, könnte aber bei der sternhöhe
-- den falschen eindruck erzeugen:
-- sieht aus wie Sigma^* = sternhöhe eins,
-- ist aber äq. zu  complement(Empty) = sternhöhe null
-- andererseits haben wir kein komplement,
-- deswegen nehmen wir All doch mit auf.

std_sigma :: NFAC c Int
	  => [c] -> E c
std_sigma alpha = E.plus std 
		$ E.make [ ( "Sigma", Basic.sigma alpha )
			 , ( "All"  , Basic.sigmastar alpha )
			 ]


--------------------------------------------------------------------------

-- | backwards compatibility
inter :: NFAC c Int
      => E c -> RX c -> NFA c Int
inter = inter_det 

-- | interprete and make deterministic and minimal
inter_det :: NFAC c Int
	  => E c -> RX c -> NFA c Int
inter_det e a = ( inter_with ( minimize . normalize ) e a )
	      { nfa_info = toDoc a }

-- | interprete but do not minimize 
inter_nondet :: NFAC c Int
	     =>  E c -> RX c -> NFA c Int
inter_nondet e a = ( inter_with ( normalize ) e a )
		   { nfa_info = toDoc a }


inter_with :: ( Symbol c, ToDoc [c], Reader [c] )
	   => (forall a . NFAC c a => NFA c a -> NFA c Int) 
	   -> E c -> RX c -> NFA c Int
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

inter_with norm e (PowerStar x)   = norm $ Ops.star    (inter_with norm e x)
inter_with norm e (PowerPlus x)   = norm $ Ops.plus    (inter_with norm e x)
inter_with norm e (Power   p x)= norm $ Ops.power p (inter_with norm e x) 


	




