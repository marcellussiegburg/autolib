-- | overlap closures

module TES.OC where

--  $Id$

import TES.Identifier
import TES.Term
import TES.Rule
import TES.Data
import TES.Position
import TES.Unify
import TES.Hull

import Util.Size
import Sets
import Data.Maybe
import Data.FiniteMap
import Control.Monad ( guard )
import Data.Char
import Data.List ( nub )

-- | 
down :: TRSC Int c 
	=> Rule Int c -> Rule Int c 
	-> [ Rule Int c ]
down (l1, r1) (l2, r2) = do
    let vs1 = vars l1 `union` vars r1
	m1 = maximum $ 0 : setToList vs1
	vs2 = vars l2 `union` vars r2
	m2 = minimum $ 0 : setToList vs2
	f v = succ v + m1 - m2
	-- construct renamed copy of (l2, r2)
	-- vars (l1, r1) is disjoint from vars (l2', r2')
	( l2', r2' ) = ( vmap f l2, vmap f r2 )
    ( p, s ) <- positions r1
    -- guard $ not $ isvar s
    u <- maybeToList $ mgu s l2'
    return $ normalize
	   $ ( apply_partial u $ l1 
	     , apply_partial u $ poke r1 (p, r2')
	     )

up :: TRSC Int c 
	=> Rule Int c -> Rule Int c 
	-> [ Rule Int c ]
up x y = let flip (l,r) = (r,l)
	 in  map flip $ down (flip y) (flip x)

inside :: TRSC Int c 
       => (  Rule Int c -> Rule Int c -> [ Rule Int c ] )
       -> (  Rule Int c -> Rule Int c -> [ Rule Int c ] )
inside op = \ x y -> do
    ( l, r ) <- op x y
    ( p, r' ) <- positions r
    return $ normalize ( l, r' )

combine :: TRSC Int c 
	=> Rule Int c -> Rule Int c 
	-> [ Rule Int c ]
combine x y = down x y ++ up x y

-- | so that vars are [0 ..]
normalize :: ( TRSC v c, TRSC Int c )
	  => Rule v c -> Rule Int c
normalize (l, r) = 
    let -- variables in order of occurences
	fm = listToFM 
	   $ zip ( nub $ voccs r ++ voccs l ) [ 0 .. ]
    in  ( applyvar fm l , applyvar fm r )
	




-- | list of overlap closures
ocs :: TRSC v c
    => TRS v c 
    -> [ Rule Int c ]
ocs trs = -- full_hull combine
          single_hull combine
	$ map normalize
	$ rules trs 

-- | list of forward closures
fcs :: TRSC v c
    => TRS v c 
    -> [ Rule Int c ]
fcs trs = do
    limit <- [ 1 .. ]
    burn ( \ ( l, r ) -> do
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



----------------------------------------------------------------

check :: [ Rule Int Identifier ]
check = combine rule1 rule2

rule1, rule2 :: Rule Int Identifier		  
rule1 = ( mv $ read "f(f(0))" , mv $ read "g(g(0))" )
rule2 = ( mv $ read "g(h(0))" , mv $ read "h(h(h(0)))" )

mv :: Term c Identifier -> Term Int Identifier
mv ( Node i [] ) | all isDigit ( name i ) = Var $ read $ name i
mv ( Node f args ) = Node f ( map mv args )
















