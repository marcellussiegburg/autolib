module Rewriting.Automaton where

--  $Id$

import qualified Autolib.NFTA
import qualified Autolib.NFTA.Basic
import qualified Autolib.NFTA.Normalize
import qualified Autolib.NFTA.Compact
import qualified Autolib.NFTA.Epsilon

import qualified Autolib.NFA
import qualified Autolib.NFA.Basic
import qualified Autolib.NFA.Normalize

import SRS.Aged
import Autolib.Symbol
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash
import Autolib.Sets
import Autolib.Util.Size

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
    lstates  :: ( FAC c s ) => a c s -> [s]
    statemap :: ( FAC c s, FAC c t ) => ( s -> t ) -> a c s -> a c t
    alphamap :: ( FAC c s, FAC d s ) => ( c -> d ) -> a c s -> a d s
    complete :: ( FAC c Int ) => Set c -> a c Int
    normalize :: ( FAC c s, FAC c Int ) => a c s -> a c Int

    compact   :: ( FAC c s ) => a c s -> a c s
    compact = id

instance Automaton NFTA.NFTA where
    lstates  = NFTA.lstates
    statemap = NFTA.statemap
    alphamap = NFTA.alphamap
    complete = NFTA.Basic.complete
    normalize = NFTA.Normalize.normalize
    compact  = NFTA.Compact.compact . NFTA.Epsilon.uneps

instance Automaton NFA.NFA where
    lstates  = NFA.lstates
    statemap = NFA.statemap
    alphamap = NFA.alphamap
    complete = NFA.Basic.sigmastar . setToList
    normalize = NFA.Normalize.normalize
