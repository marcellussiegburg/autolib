-- | overlap closures

module TES.OC where

--  $Id$

import TES.Identifier
import TES.Term
import TES.Rule
import TES.Position
import TES.Unify

import Sets
import Data.Maybe
import Control.Monad ( guard )

-- | 
combine :: TRSC Int c 
	=> Rule Int c -> Rule Int c 
	-> [ Rule Int c ]
combine (l1, r1) (l2, r2) = do
    let vs1 = vars l1 `union` vars r1
	m1 = maximum $ 0 : setToList vs1
	vs2 = vars l2 `union` vars r2
	m2 = minimum $ 0 : setToList vs2
	f v = succ v + m1 - m2
	-- construct renamed copy of (l2, r2)
	-- vars (l1, r1) is disjoint from vars (l2', r2')
	( l2', r2' ) = ( vmap f l2, vmap f r2 )
    ( p, s ) <- positions r1
    guard $ not $ isvar s
    u <- maybeToList $ mgu s l2'
    return ( apply_partial u $ l1 
	   , apply_partial u $ poke r1 (p, r2')
	   )


check :: [ Rule Int Identifier ]
check = combine rule1 rule2

rule1, rule2 :: Rule Int Identifier		  
rule1 = ( read "f(f(0))" , read "g(g(0))" )
rule2 = ( read "g(h(0))" , read "h(h(h(0)))" )
















