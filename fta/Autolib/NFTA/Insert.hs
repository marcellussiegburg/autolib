module NFTA.Insert 

--  $Id$

( insert )

where

import NFTA.Type
import TES.Term

import Data.List ( partition )
import Control.Monad.State

-- | add transitions (using all new states)
insert :: NFTAC c Int 
       => NFTA c Int
       -> (Int, Term Int c)
       -> NFTA c Int
insert ( a :: NFTA c Int ) pt = evalState 
       ( do ins pt ; (a, n) <- get ; return a )
       ( a , succ $ maximum $ lstates a )

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
    add ( p, c, qs )
    return p

ins (p, Var q) = do
    ( a, n ) <- get
    put ( add_epsilon a (p, q) , n )
    return p

next :: NFTAC c Int 
     =>  ST c Int
next = do 
    ( a, n ) <- get
    put ( a { states = states a `union` mkSet [n] }
	, succ n 
	)
    return n

add :: NFTAC c Int
    => ( Int, c, [Int] ) 
    -> ST c ()
add t = do 
    ( a, n ) <- get
    put ( a { trans = trans a `union` mkSet [t] } 
	, n 
	)    

add_epsilon :: NFTAC c s
	    => NFTA c s
	    -> ( s, s ) -- ^ ( from state , to state )
	    -> NFTA c s
add_epsilon a (p, q) = 
    a { trans = trans a `union` mkSet ( do
          ( x, c, ys ) <- ltrans a
	  guard $ x == p
	  return ( q, c, ys )
	)
      }