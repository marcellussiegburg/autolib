--  $Id$

module Autolib.NFA.Type

( module Autolib.NFA.Type
, module Autolib.NFA.Data
)

where

import Autolib.Set
import Autolib.Letters

import Autolib.NFA.Data

import Control.Monad (guard)
import Autolib.ToDoc

---------------------------------------------------------------------

-- | find list of states that are reached after one step
images :: NFAC c s
       => NFA c s -> (s, c) -> [ s ]
images a (p, c) =
    case lookupFM (trans a) (p, c) of
         Nothing -> []
	 Just qs -> setToList qs


---------------------------------------------------------------------

-- | list of states
lstates :: NFAC c s  
	=> NFA c s -> [ s ]
lstates = setToList . states

-- | list of start states
lstarts :: NFAC c s  
        => NFA c s -> [ s ]
lstarts = setToList . starts

-- | list of final states
lfinals :: NFAC c s
        => NFA c s -> [ s ]
lfinals = setToList . finals

-- | transition function as list of pairs
ltrans :: NFAC c s
       => NFA c s -> [ ((s, c),(Set s)) ]
ltrans  = fmToList  . trans

instance NFAC c s => Letters (NFA c s) c where
   -- letters a = mkSet $ do ((p, c), qs) <- ltrans a; return c
   letters = alphabet

---------------------------------------------------------------------

-- | accepts epsilon?
ceps :: NFAC c s => NFA c s -> Bool
ceps a = not $ isEmptySet $ intersect (starts a) (finals a)

---------------------------------------------------------------------

-- | set of states that is reachable via labelled path
-- computation runs in polynopmial time
reaches :: NFAC c s => NFA c s -> [c] -> Set s
reaches a w = rch w (starts a)
    where rch [] qs = qs
	  rch (c : cs) qs = 
	      let ps = mkSet $ do
		     q <- setToList qs
		     setToList $ lookupWithDefaultFM (trans a) emptySet (q, c)
	      in  rch cs ps

-- | word is accepted by automaton?
-- runs in polynomial time
is_accepted_by :: NFAC c s => [c] -> NFA c s -> Bool
is_accepted_by w a = 
    not $ isEmptySet $ intersect (reaches a w) (finals a)	  

---------------------------------------------------------------------

-- | apply function to states
statemap ::  (NFAC c s, NFAC c t)
	 => (s -> t) -> (NFA c s -> NFA c t)
statemap f n =
    NFA { nfa_info = funni "statemap" [ info f , info n ]
        , alphabet = alphabet n
	, states = smap f $ states n
	, starts = smap f $ starts n
	, finals = smap f $ finals n
	, trans = collect $ do
	    (p, c, q) <- unCollect $ trans n
	    return ( f p, c, f q ) 
	}

-- | build sub-automaton consisting of states
-- that fulfil criterion
-- note: alphabet is just copied (may be too large)
statefilter :: NFAC c s
	=> (s -> Bool) -> NFA c s -> NFA c s
statefilter f a =
    	 a { nfa_info = funni "statefilter" [ info f , info a ]
	   , alphabet = alphabet a
	   , states = sfilter f $ states a
	   , finals = sfilter f $ finals a
	   , starts = sfilter f $ starts a
	   , trans = collect $ do 
	        (p, c, q) <- unCollect $ trans a
	        guard $ f p && f q
	        return (p, c, q)
	   } 

---------------------------------------------------------------------

-- | apply function to letters
alphamap :: ( NFAC c s, NFAC d s )
	    => (c -> d) -> (NFA c s -> NFA d s)
alphamap f n =
    NFA { nfa_info = funni "alphamap" [ info f , info n ]
	, alphabet = smap f $ alphabet n
	, states = states n
	, starts = starts n
	, finals = finals n
	, trans = collect $ do
	    (p, c, q) <- unCollect $ trans n
	    return ( p, f c, q ) 
	}

-- | keep only those transitions labelled with letters
-- that fulfil criterion.
-- note: alphabet and states are just copied
-- (some may be unreachable)
alphafilter :: NFAC c s
	=> (c -> Bool) -> NFA c s -> NFA c s
alphafilter f a 
    = informed ( funni "alphafilter" [ info f , info a ] )
    $ transfilter ( \ (p, c, q) -> f c ) a 
    { alphabet = sfilter f $ alphabet a }

---------------------------------------------------------------------

-- | keep only transitions that fulfil criterion.
-- note: alphabet and states are just copied
-- (some may be unreachable)
transfilter :: NFAC c s
	    => ((s, c, s) -> Bool)
	    -> NFA c s
	    -> NFA c s
transfilter f a =
    	 a { nfa_info = funni "transfilter" [ info f , info a ]
	   , alphabet = alphabet a
	   , states = states a
	   , finals = finals a
	   , starts = starts a
	   , trans = collect $ do 
	        t <- unCollect $ trans a
	        guard $ f t
	        return t
	   } 


