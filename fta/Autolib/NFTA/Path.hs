-- | the path automaton contains two kinds of states:
-- state nodes and transition nodes
--
-- a transition node q0 -> c(q1, .., qn) 
-- is labelled with c
-- and has an incoming edge from the state node q0
-- and outgoing edges labelled i to state nodes qi

module Autolib.NFTA.Path where

--  $Id$

import qualified Autolib.NFTA.Type as T -- trees
import qualified Autolib.NFA.Type as W -- words

import Autolib.Sets
import Autolib.ToDoc
import Autolib.Reader

import Autolib.Symbol
import Autolib.Util.Size
import Autolib.Hash
import Autolib.Informed

----------------------------------------------------------------------------

make :: ( T.NFTAC c s )
     => T.NFTA c s
     -> W.NFA Edge (Node c s)
make t = 
    let states = T.lstates t
	trans  = T.ltrans  t
        ar     = maximum $ map arity $ setToList $ T.alphabet t
    in  W.NFA { W.nfa_info = funni "Autolib.NFTA.Path.make" [ info t ]
	      , W.alphabet = mkSet $ Nil : map Edge [ 1 .. ar ]
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

instance ( Hash c, Hash s ) => Hash ( Node c s ) where
    hash ( Transition t ) = hash t
    hash ( State s ) = hash s

data Edge = Edge Int
	  | Nil
     deriving ( Eq, Ord )

instance Show Edge where
    show (Edge i) = show i
    show Nil = ""

instance Hash ( Edge ) where
    hash ( Edge x ) = hash x
    hash Nil = 4711

instance Reader Edge -- no methods
instance ToDoc Edge 

instance Symbol Edge -- dummy




