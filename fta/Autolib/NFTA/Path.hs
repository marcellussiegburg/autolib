-- | the path automaton contains two kinds of states:
-- state nodes and transition nodes
--
-- a transition node q0 -> c(q1, .., qn) 
-- is labelled with c
-- and has an incoming edge from the state node q0
-- and outgoing edges labelled i to state nodes qi

module NFTA.Path where

--  $Id$

import qualified NFTA.Type as T -- trees
import qualified NFA.Type as W -- words

import Sets
import ToDoc
import Reader

import TES.Symbol

----------------------------------------------------------------------------

instance Symbol Edge

make :: ( T.NFTAC c s )
     => T.NFTA c s
     -> W.NFA Edge (Node c s)
make t = 
    let states = T.lstates t
	trans  = T.ltrans  t
    in  W.NFA { W.nfa_info = text "NFTA.Path.make ()"
	      , W.states = mkSet $  map State      states
		                 ++ map Transition trans 
	      , W.starts = smap State $ T.finals t
	      , W.finals = smap State $ T.states t
	      , W.trans  = W.collect $ 
                    do ( p, q ) <- T.leps t
		       return ( State p, Nil, State q )
		 ++ do x @ ( q, c, qs ) <- trans
		       ( State q, Nil, Transition x ) : do
		           (i, q) <- zip [ 1 .. ] qs
		           return ( Transition x, Edge i, State q )
	      }

----------------------------------------------------------------------------

data Node c s = Transition ( s, c, [s] )
	      | State s
     deriving ( Eq, Ord )

instance ( Show c, Show s ) => Show (Node c s) where
    show ( Transition (q, c, qs) ) = show c
    show ( State s ) = show s

instance Reader (Node c s) -- no methods
instance ToDoc (Node c s)

data Edge = Edge Int
	  | Nil
     deriving ( Eq, Ord )

instance Show Edge where
    show (Edge i) = show i
    show Nil = ""

instance Reader Edge -- no methods
instance ToDoc Edge 

