-- | insert new transitions (and states) into an automaton

module Autolib.NFTA.Insert 

--  $Id$

( inserts, insert
, extends
)

where

import Autolib.NFTA.Type
import Autolib.TES.Term
import Autolib.Rewriting.Path

import qualified Autolib.Relation as Relation
import Data.List ( partition )

import Control.Monad.State

import Autolib.Reader
import Autolib.ToDoc
import Autolib.FiniteMap



-- | add transitions (using all new states)
inserts :: NFTAC c Int 
       => NFTA c Int
       -> [ (Int, Term Int c) ] -- ^ ( top, term )
       -> NFTA c Int
inserts a pqs = run 
	      $ internal ( succ $ maximum $ lstates a )
	                 ( \ c -> do ( a, n ) <- get
		                     put (a, succ n)
		                     return n
		         )
	                 id
	                 a
			 pqs


insert :: NFTAC c Int 
       => NFTA c Int
       -> (Int, Term Int c) -- ^ ( top, term )
       -> NFTA c Int
insert a pt = inserts a [ pt ]

{-# inline extends #-}

-- | add transitions (that contains symbols annotated with states)
extends :: ( NFTAC c s , TRSC v (c, s)
	   , Show s, ToDoc ( FiniteMap v s ) 
	   )
       => NFTA c s
       -> [ Path Term v (c, s) s ]
       -> NFTA c s
extends a paths = run $ internal 
           ()
	   ( \ (c, s) -> do return s )
           ( \ (c, s) -> c )
           a
	   ( do p <- paths
	        return ( from p, applyvar (to p) (walk p) )
	   )

run :: State s x -> x
run action = evalState action undefined

----------------------------------------------------------------

internal :: NFTAC c s
	 => t -- ^ initial accu
	 -> ( d -> State ( NFTA c s , t ) s ) -- ^ get new state
	 -> ( d -> c ) -- ^ get real symbol
	 -> NFTA c s
	 -> [ ( s, Term s d ) ]
	 -> State (NFTA c s, t) ( NFTA c s )
	 
internal ( state0 :: t ) gen pick ( a :: NFTA c s ) pts = do 
     put ( a, state0 )
     mapM_ ( ins gen pick ) pts 
     (a, _) <- get 
     return a 

ins :: NFTAC c s
    => ( d -> State ( NFTA c s, t ) s )
    -> ( d -> c )
    -> ( s, Term s d ) 
    -> State (NFTA c s, t) s
ins gen pick (p, Node c args ) = do
    qs  <- mapM ( \ arg -> case arg of
			 Var v -> return v
			 Node c' _ -> do 
			      n <- gen c'
                              add_state n
			      ins gen pick (n, arg)
		 ) args
    add_trans ( p, pick c, qs )
    return p

ins gen pick (p, Var q) = do
    ( a, n ) <- get
    add_eps (p, q)
    return p

--------------------------------------------------------------------

{-# INLINE add_state #-}

add_state :: NFTAC c s
     => s -> State ( NFTA c s, t) ()
add_state s = do 
    ( a, t ) <- get
    put ( a { states = states a `union` unitSet s }
	, t
	)


-------------------------------------------------------------------

{-# INLINE add_trans #-}

-- | add transition
add_trans :: NFTAC c s
    => ( s, c, [s] ) 
    -> State (NFTA c s, t) ()
add_trans t @ ( p, c, qs ) = do 
    ( a, n ) <- get
    put ( a { alphabet = union (alphabet a) $ unitSet c
	    , states = union ( states a ) $ unitSet p
	    , trans = Relation.insert (trans a) ( p, (c, qs)) 
	    }
	, n 
	)    

{-# INLINE add_eps #-}

-- | add epsilon transition
add_eps :: NFTAC c s
    => ( s, s ) 
    -> State ( NFTA c s, t) ()
add_eps (x,y) = do 
    ( a, n ) <- get
    put ( a { eps = Relation.trans $ Relation.insert (eps a) (x,y) 
	    } 
	, n 
	)    

