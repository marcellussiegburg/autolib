module Rewriting.Automaton where

--  $Id$

import qualified NFTA
import qualified NFTA.Basic
import qualified NFTA.Normalize
import qualified NFA
import qualified NFA.Basic
import qualified NFA.Normalize

import SRS.Aged
import TES.Symbol
import ToDoc
import Reader
import Hash
import Sets
import Util.Size

class ( Symbol c, ToDoc [c]
      , Ord s , ToDoc s
      , ToDoc [s], Reader [s]
      , Reader s
      , Hash c, Hash s
      ) => FAC c s

instance ( Symbol c, ToDoc [c]
      , Ord s , ToDoc s
      , ToDoc [s], Reader [s]
      , Reader s
      , Hash c, Hash s
      ) => FAC c s

class  Automaton a where
    statemap :: ( FAC c s, FAC c t ) => ( s -> t ) -> a c s -> a c t
    alphamap :: ( FAC c s, FAC d s ) => ( c -> d ) -> a c s -> a d s
    complete :: ( FAC c Int ) => Set c -> a c Int
    normalize :: ( FAC c s, FAC c Int ) => a c s -> a c Int

instance Automaton NFTA.NFTA where
    statemap = NFTA.statemap
    alphamap = NFTA.alphamap
    complete = NFTA.Basic.complete
    normalize = NFTA.Normalize.normalize

instance Automaton NFA.NFA where
    statemap = NFA.statemap
    alphamap = NFA.alphamap
    complete = NFA.Basic.sigmastar . setToList
    normalize = NFA.Normalize.normalize
