module Autolib.NFA.Compact where

--  $Id$

import Autolib.NFA.Type
import Autolib.Exp.Type
import Autolib.Exp.Read
import Autolib.Exp.Print
import Autolib.Exp.Syntax
import Autolib.Symbol

import Autolib.FiniteMap
import Autolib.Set
import Autolib.Letters
import Control.Monad ( guard )


import Autolib.Reader
import Autolib.ToDoc

make :: ( NFAC c s, NFAC [c] s, NFAC [[c]] s, NFAC (RX c) s )
     => NFA c s
     -> NFA (RX c) s
make  = alphamap rxify
      . parallel
      . sequential

instance Symbol c => Symbol (RX c)
instance ( Symbol c, Reader [c], ToDoc [c] ) => Symbol [c]

-- | fold into regexp
rxify :: Symbol c => [[c]] -> RX c
rxify = foldr1 Union 
      . map ( foldr1 Dot )
      . map ( map Letter )

-- | collect all edges between same pair of states
parallel :: ( NFAC c s, NFAC [c] s )
		 => NFA c s -> NFA [c] s
parallel a = 
    let accu = addListToFM_C union emptyFM $ do
	    ( p, c, q ) <- unCollect $ trans a
	    return ( (p, q), unitSet c )
	trs = do
	          (( p, q ), cs ) <- fmToList accu
	          return ( p, setToList cs, q )
        lts = do ( p, c, q ) <- trs ; return c
    in  a   { nfa_info = funni "parallel_compact" [ info a ]
	    , alphabet = mkSet lts
	    , trans = collect trs
	    }

-- | replace unique path over several states 
-- by single edge labelled with sequence of letters
sequential :: ( NFAC c s, NFAC [c] s )
	=> NFA c s -> NFA [c] s
sequential a =
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
  
        trans' = do
	    p <- setToList $ keep
	    ( cs, q ) <- paths p
	    return ( p, cs, q )
	lts = do ( p, c, q ) <- trans' ; return c

    in  NFA { nfa_info = funni "sequential_compact" [ info a ]
	    , alphabet = mkSet lts
	    , states = keep
	    , starts = starts a
	    , finals = finals a
	    , trans = collect trans'
	    }

