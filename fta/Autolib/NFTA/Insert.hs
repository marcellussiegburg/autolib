module NFTA.Insert 

--  $Id$

( insert , inserts
, links
)

where

import NFTA.Type
import TES.Term

import qualified Relation
import Data.List ( partition )
import Control.Monad.State

-- | add transitions (using all new states)
inserts :: NFTAC c Int 
       => NFTA c Int
       -> [ (Int, Term Int c) ] -- ^ ( top, term )
       -> NFTA c Int
inserts ( a :: NFTA c Int ) pts = evalState 
       ( do mapM_ ins pts ; (a, n) <- get ; return a )
       ( a , succ $ maximum $ lstates a )

insert :: NFTAC c Int 
       => NFTA c Int
       -> (Int, Term Int c) -- ^ ( top, term )
       -> NFTA c Int
insert a pt = inserts a [ pt ]


type ST c s = State ( NFTA c s, s )

ins :: NFTAC c Int
    =>  ( Int, Term Int c ) 
    -> ST c Int Int
ins (p, Node c args ) = do
    qs  <- mapM ( \ arg -> case arg of
			 Var v -> return v
			 _     -> do n <- next
				     ins (n, arg)
		 ) args
    add_trans ( p, c, qs )
    return p

ins (p, Var q) = do
    ( a, n ) <- get
    add_eps (p, q)
    return p

--------------------------------------------------------------------

-- | get a fresh state
next :: NFTAC c Int 
     =>  ST c Int Int
next = do 
    ( a, n ) <- get
    put ( a { states = states a `union` mkSet [n] }
	, succ n 
	)
    return n

-- | add transition
add_trans :: NFTAC c s
    => ( s, c, [s] ) 
    -> ST c s ()
add_trans t @ ( p, c, qs ) = do 
    ( a, n ) <- get
    put ( a { states = union ( states a ) $ unitSet p
	    , trans = Relation.insert (trans a) ( p, (c, qs)) 
	    , inv_trans = Relation.insert (inv_trans a) ( (qs, c), p) 
	    , eps = Relation.insert (eps a) (p, p)
	    , inv_eps = Relation.insert (inv_eps a) (p, p)
	    }
	, n 
	)    

-- | add epsilon transition
add_eps :: NFTAC c s
    => ( s, s ) 
    -> ST c s ()
add_eps (x,y) = do 
    ( a, n ) <- get
    put ( a { eps = Relation.trans $ Relation.insert (eps a) (x,y) 
	    ,  inv_eps = Relation.trans $ Relation.insert (inv_eps a) (y,x) 
	    } 
	, n 
	)    


links :: NFTAC c s
	  => NFTA c s
	  -> [ ( s, (c, [s])) ]
	  -> NFTA c s
links a pcqss = 
    let trs = do (p, (c, qs)) <- pcqss
		 return (p, c, qs)
    in	evalState 
       ( do mapM_ add_trans trs ; (a, n) <- get ; return a )
       ( a , error "NFTA.Insert.links should not need new states" )

