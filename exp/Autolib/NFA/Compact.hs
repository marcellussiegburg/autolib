module NFA.Compact where

-- -- $Id$

import NFA.Type

import Data.FiniteMap
import Sets
import Letters
import Control.Monad ( guard )

parallel_compact :: ( NFAC c s, NFAC [c] s )
		 => NFA c s -> NFA [c] s
-- sammelt verschieden beschriftete pfeil zwischen gleichen zuständen
parallel_compact a = 
    let accu = addListToFM_C union emptyFM $ do
	    ( p, c, q ) <- unCollect $ trans a
	    return ( (p, q), unitSet c )
    in  a   { nfa_info = funni "parallel_compact" [ info a ]
	    , trans = collect $ do
	          (( p, q ), cs ) <- fmToList accu
	          return ( p, setToList cs, q )
	    }


sequential_compact :: ( NFAC c s, NFAC [c] s )
	=> NFA c s -> NFA [c] s
-- ersetze Pfade im automaten durch pfeile mit wörtern dran
sequential_compact a =
    let sigma = letters a

        indegrees  = addListToFM_C (+) emptyFM $ do
	    (p, c, q) <- unCollect $ trans a
	    return (q, 1)
	outdegrees = addListToFM_C (+) emptyFM $ do
	    (p, c, q) <- unCollect $ trans a
	    return (p, 1)

	-- genau diese zustände wollen wir auch zum schluß sehen
	keep = mkSet $ do 
	    p <- lstates a
	    guard $  p `elementOf` starts a
		  || p `elementOf` finals a
		  || Just 1 /= lookupFM  indegrees p
		  || Just 1 /= lookupFM outdegrees p
	    return p
	keeping p = p `elementOf` keep

        -- maximale pfade (zu keeping-zielen)
        paths p = do 
	      c <- setToList sigma
	      q <- setToList $ lookupset (trans a) (p, c)
	      if keeping q
		 then do -- hier schluß
		      return ( [c], q )
		 else do -- weiter
		      ( cs, r ) <- paths q
	              return (c : cs, r)
  
        trans' = collect $ do
	    p <- setToList $ keep
	    ( cs, q ) <- paths p
	    return ( p, cs, q )

    in  NFA { nfa_info = funni "sequential_compact" [ info a ]
	    , states = keep
	    , starts = starts a
	    , finals = finals a
	    , trans = trans'
	    }


lookupset fm = lookupWithDefaultFM fm emptySet
