-- $Id$

module NFA.Trim  

( trim
, reachable
, productive
)

where

import NFA.Type

import Monad (guard)
import Fix
import Sets
import ToDoc

reachable_states :: NFAC c s => NFA c s -> Set s 
reachable_states a =
    fix ( \ xs -> union xs $ mkSet $ do 
	    (p, c, q) <- unCollect $ trans a
	    guard $ p `elementOf` xs
	    return q
	) ( starts a )

reachable :: NFAC c s => NFA c s -> NFA c s
reachable a = 
    let xs = reachable_states a
    in	( statefilter (`elementOf` xs) a )
	{ nfa_info = funni "reachable" [ info a ] }


productive_states :: NFAC c s => NFA c s -> Set s
productive_states a =
    fix ( \ xs -> union xs $ mkSet $ do
	    (p, c, q) <- unCollect $ trans a
	    guard $ elementOf q xs 
	    return p
	) ( finals a )

productive :: NFAC c s => NFA c s -> NFA c s
productive a = 
    let xs = productive_states a
    in	( statefilter (`elementOf` xs) a )
	{ nfa_info = funni "productive" [ info a ] }

trim :: NFAC c s => NFA c s -> NFA c s
trim a = ( reachable $ productive a ) 
	 { nfa_info = funni "trim" [ info a ] }
