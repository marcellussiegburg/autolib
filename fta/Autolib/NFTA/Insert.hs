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


type ST c = State ( NFTA c Int, Int )

ins :: NFTAC c Int
    =>  ( Int, Term Int c ) 
    -> ST c Int
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
     =>  ST c Int
next = do 
    ( a, n ) <- get
    put ( a { states = states a `union` mkSet [n] }
	, succ n 
	)
    return n

-- | add transition
add_trans :: NFTAC c Int
    => ( Int, c, [Int] ) 
    -> ST c ()
add_trans t @ ( p, c, qs ) = do 
    ( a, n ) <- get
    put ( a { trans = Relation.insert (trans a) ( p, (c, qs)) 
	    , inv_trans = Relation.insert (inv_trans a) ( (qs, c), p) 
	    , eps = Relation.insert (eps a) (p, p)
	    , inv_eps = Relation.insert (inv_eps a) (p, p)
	    }
	, n 
	)    

-- | add epsilon transition
add_eps :: NFTAC c Int
    => ( Int, Int ) 
    -> ST c ()
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
links a pcqs = 
    let sts = mkSet $ do (p,(c,qs)) <- pcqs ; p : qs
    in a { states = states a `union` sts
	 , trans = Relation.plus ( trans a ) ( Relation.make pcqs )
	 , inv_trans = Relation.plus ( inv_trans a ) 
		 ( Relation.make $ map (\ (p,(c,qs))->((qs,c),p))  pcqs )
	 }	 