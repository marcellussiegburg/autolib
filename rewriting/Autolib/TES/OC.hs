-- | overlap closures

module Autolib.TES.OC where

--  $Id$

import Autolib.TES.Identifier
import Autolib.TES.Term
import Autolib.TES.Rule
import Autolib.TES.Data
import Autolib.TES.Position
import Autolib.TES.Unify
import Autolib.TES.Hull

import Autolib.Util.Size
import Autolib.Set
import Data.Maybe
import Autolib.FiniteMap
import Control.Monad ( guard )
import Data.Char
import Data.List ( nub )

-- | 
down :: TRSC Int c 
	=> Rule (Term Int c) -> Rule (Term Int c)
	-> [ Rule (Term Int c) ]
down (Rule {lhs = l1, strict = s1, rhs = r1}) 
     (Rule { lhs = l2, strict = s2, rhs = r2}) = do
    let vs1 = vars l1 `union` vars r1
	m1 = maximum $ 0 : setToList vs1
	vs2 = vars l2 `union` vars r2
	m2 = minimum $ 0 : setToList vs2
	f v = succ v + m1 - m2
	-- construct renamed copy of (l2, r2)
	-- vars (l1, r1) is disjoint from vars (l2', r2')
	( l2', r2' ) = ( vmap f l2, vmap f r2 )
    ( p, s ) <- positions r1

    -- this was commented out? why?
    -- check  "(VAR x y z) (RULES a -> foo  a -> bar  f(x,y,x,y,z)->f(foo,bar,z,z,z) )"
    guard $ not $ isvar s

    u <- maybeToList $ mgu s l2'
    return $ normalize
	   $ Rule { lhs =  apply_partial u $ l1 
		  , strict = s1 || s2
		  , rhs = apply_partial u $ poke r1 (p, r2')
		  }

up :: TRSC Int c 
	=> Rule (Term Int c) -> Rule (Term Int c) 
	-> [ Rule (Term Int c) ]
up x y = let flip rule = rule { lhs = rhs rule, rhs = lhs rule }
	 in  map flip $ down (flip y) (flip x)

inside :: TRSC Int c 
       => (  Rule (Term Int c) -> Rule (Term Int c) -> [ Rule (Term Int c) ] )
       -> (  Rule (Term Int c) -> Rule (Term Int c) -> [ Rule (Term Int c) ] )
inside op = \ x y -> do
    rule <- op x y
    ( p, r' ) <- positions $ rhs rule
    return $ normalize $ rule { rhs = r' }

combine :: TRSC Int c 
	=> Rule (Term Int c) -> Rule (Term Int c) 
	-> [ Rule (Term Int c) ]
combine x y = down x y ++ up x y

-- | so that vars are [0 ..]
normalize :: ( TRSC v c, TRSC Int c )
	  => Rule (Term v c) -> Rule (Term Int c)
normalize (rule @ Rule { lhs = l, strict = s, rhs = r}) = 
    let -- variables in order of occurences
	fm = listToFM 
	   $ zip ( nub $ voccs r ++ voccs l ) [ 0 .. ]
    in  Rule { lhs = applyvar fm l
	     , strict = s
	     , rhs = applyvar fm r
	     }

-- | list of overlap closures
ocs :: TRSC v c
    => TRS v c 
    -> [ Rule (Term Int c) ]
ocs trs = -- full_hull combine
          -- single_hull combine
          binary_hull combine
	$ map normalize
	$ rules trs 

-- | list of forward closures
fcs :: TRSC v c
    => TRS v c 
    -> [ Rule (Term Int c) ]
fcs trs = do
    limit <- [ 1 .. ]
    burn ( \ ( Rule { lhs = l, rhs = r } ) -> do
	       let sl =  size l ; sr = size r
	       guard $ sl + sr < limit
	       -- guard $ ( length (voccs r) > length (lvars r) )
	       return $ sl + sr ^ 2
	    ) 
	    -- ( \ (l, r) -> (size r, r ) ) -- tagging
	    ( id ) -- no tagging
	    ( down )
	$ map normalize
	$ rules trs 
