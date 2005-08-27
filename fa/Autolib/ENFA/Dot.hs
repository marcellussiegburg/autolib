{-# OPTIONS -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Autolib.ENFA.Dot () where

--  $Id$

import Autolib.ENFA.Data
import Autolib.Dot.Dot
import Autolib.Symbol
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Autolib.Hash
import qualified Autolib.NFA as N
import qualified Autolib.Relation as R

data Letter c = Epsilon | Single c
    deriving ( Eq, Ord )

instance Show c => Show ( Letter c ) where
    show ( Epsilon )  = "eps"
    show ( Single c ) = show c

instance Reader ( Letter c ) -- dummy

instance Hash c => Hash ( Letter c ) where
    hash Epsilon = 13
    hash (Single c) = hash c

instance Size c => Size ( Letter c ) where
    size _ = 1

instance Symbol c => Symbol ( Letter c )

to_NFA :: NFAC c a => ENFA c a -> N.NFA ( Letter c ) a
to_NFA a = N.NFA
	 { N.nfa_info = enfa_info a
         , N.alphabet = mkSet 
		      $ Epsilon : map Single ( setToList $ alphabet a )
	 , N.states = states a
	 , N.starts = starts a
         , N.finals = finals a
	 , N.trans  = collect $  do (p, c, q ) <- tunCollect ( trans a )
				    return ( p, Single c, q )
			      ++ do ( p, q ) <- R.pairs ( eps a )
				    return ( p, Epsilon, q )
	 }

instance ( NFAC c a, Show a, Show c ) 
     => ToDot ( ENFA c a ) where
    toDot = toDot . to_NFA
    toDotProgram = toDotProgram . to_NFA
    toDotOptions = toDotOptions . to_NFA

