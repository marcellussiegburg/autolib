--  $Id$

module Autolib.NFA.Type

( module Autolib.NFA.Type
, module Autolib.NFA.Data
)

where

import Autolib.Sets
import Autolib.Letters

import NFA.Data

import Control.Monad (guard)
import Autolib.ToDoc

---------------------------------------------------------------------

collect :: NFAC c s
	=> [(s, c, s)] -> FiniteMap (s, c) (Set s)
collect pxqs = addListToFM_C union emptyFM $ do
    (p, x, q) <- pxqs
    return ((p, x), unitSet q)

images :: NFAC c s
       => NFA c s -> (s, c) -> [ s ]
images a (p, c) =
    case lookupFM (trans a) (p, c) of
         Nothing -> []
	 Just qs -> setToList qs

unCollect :: FiniteMap (s, c) (Set s) -> [(s, c, s)]
unCollect fm = do
    ((p,c), qs) <- fmToList fm
    q <- setToList qs
    return (p, c, q)

---------------------------------------------------------------------

lstates :: NFAC c s  
	=> NFA c s -> [ s ]
lstates = setToList . states
lstarts :: NFAC c s  
        => NFA c s -> [ s ]
lstarts = setToList . starts
ltrans :: NFAC c s
       => NFA c s -> [ ((s, c),(Set s)) ]
ltrans  = fmToList  . trans
lfinals :: NFAC c s
        => NFA c s -> [ s ]
lfinals = setToList . finals

instance NFAC c s => Letters (NFA c s) c where
   letters a = mkSet $ do ((p, c), qs) <- ltrans a; return c

---------------------------------------------------------------------

ceps :: NFAC c s => NFA c s -> Bool
-- accepts epsilon?
ceps a = not $ isEmptySet $ intersect (starts a) (finals a)

---------------------------------------------------------------------

-- polynomialzeit

reaches :: NFAC c s => NFA c s -> [c] -> Set s
reaches a w = rch w (starts a)
    where rch [] qs = qs
	  rch (c : cs) qs = 
	      let ps = mkSet $ do
		     q <- setToList qs
		     setToList $ lookupWithDefaultFM (trans a) emptySet (q, c)
	      in  rch cs ps

is_accepted_by :: NFAC c s => [c] -> NFA c s -> Bool
is_accepted_by w a = 
    not $ isEmptySet $ intersect (reaches a w) (finals a)	  

---------------------------------------------------------------------

statemap ::  (NFAC c s, NFAC c t)
	 => (s -> t) -> (NFA c s -> NFA c t)
statemap f n =
    NFA { nfa_info = funni "statemap" [ text "<<function>>" , info n ]
	, states = smap f $ states n
	, starts = smap f $ starts n
	, finals = smap f $ finals n
	, trans = collect $ do
	    (p, c, q) <- unCollect $ trans n
	    return ( f p, c, f q ) 
	}

statefilter :: NFAC c s
	=> (s -> Bool) -> NFA c s -> NFA c s
statefilter f a =
    	 a { nfa_info = funni "statefilter" [ text "<<predicate>>" , info a ]
	   , states = sfilter f $ states a
	   , finals = sfilter f $ finals a
	   , starts = sfilter f $ starts a
	   , trans = collect $ do 
	        (p, c, q) <- unCollect $ trans a
	        guard $ f p && f q
	        return (p, c, q)
	   } 

---------------------------------------------------------------------

alphamap :: ( NFAC c s, NFAC d s )
	    => (c -> d) -> (NFA c s -> NFA d s)
alphamap f n =
    NFA { nfa_info = funni "alphamap" [ text "<<function>>" , info n ]
	, states = states n
	, starts = starts n
	, finals = finals n
	, trans = collect $ do
	    (p, c, q) <- unCollect $ trans n
	    return ( p, f c, q ) 
	}

alphafilter :: NFAC c s
	=> (c -> Bool) -> NFA c s -> NFA c s
alphafilter f a 
    = informed ( funni "alphafilter" [ text "<<predicate>>" , info a ] )
    $ transfilter ( \ (p, c, q) -> f c ) a 

---------------------------------------------------------------------

transfilter :: NFAC c s
	    => ((s, c, s) -> Bool)
	    -> NFA c s
	    -> NFA c s
transfilter f a =
    	 a { nfa_info = funni "transfilter" [ text "<<predicate>>" , info a ]
	   , states = states a
	   , finals = finals a
	   , starts = starts a
	   , trans = collect $ do 
	        t <- unCollect $ trans a
	        guard $ f t
	        return t
	   } 


