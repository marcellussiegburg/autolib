module Autolib.Rewriting.Automaton where

--  $Id$

import qualified Autolib.NFTA
import qualified Autolib.NFTA.Basic
import qualified Autolib.NFTA.Ops
import qualified Autolib.NFTA.Trim
import qualified Autolib.NFTA.Normalize
import qualified Autolib.NFTA.Compact
import qualified Autolib.NFTA.Complement
import qualified Autolib.NFTA.Epsilon

import qualified Autolib.NFA
import qualified Autolib.NFA.Basic
import qualified Autolib.NFA.Ops
import qualified Autolib.NFA.Trim
import qualified Autolib.NFA.Minus
import qualified Autolib.NFA.Normalize


import Autolib.Symbol
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash
import Autolib.Sets
import Autolib.Letters
import Autolib.Util.Size

class ( Symbol c, ToDoc [c], Reader [c]
      , Ord s , ToDoc s
      , ToDoc [s], Reader [s]
      , Reader s
      , Hash c, Hash s
      ) => FAC c s

instance ( Symbol c, ToDoc [c], Reader [c]
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

    -- | identify some equivalent states
    compact   :: ( FAC c s ) => a c s -> a c s
    compact = id
 
    -- | keep only useful (reachable, productive) states
    trim      :: ( FAC c s ) => a c s -> a c s
    
    intersection :: ( FAC c s, FAC c t, FAC c Int ) 
		 => a c s -> a c t -> a c Int
    complement :: ( FAC c s , FAC c Int )
               => a c s -> a c Int


instance Automaton Autolib.NFTA.NFTA where
    lstates  = Autolib.NFTA.lstates
    statemap = Autolib.NFTA.statemap
    alphamap = Autolib.NFTA.alphamap
    complete = Autolib.NFTA.Basic.complete
    normalize = Autolib.NFTA.Normalize.normalize
    compact  = Autolib.NFTA.Compact.compact . Autolib.NFTA.Epsilon.uneps
    trim     = Autolib.NFTA.Trim.trim . Autolib.NFTA.Epsilon.uneps
    intersection a b = Autolib.NFTA.Normalize.normalize
                     $ Autolib.NFTA.Ops.intersection 
                         ( Autolib.NFTA.Epsilon.uneps a ) 
			 ( Autolib.NFTA.Epsilon.uneps b )
    complement = Autolib.NFTA.Complement.complement

instance Automaton Autolib.NFA.NFA where
    lstates  = Autolib.NFA.lstates
    statemap = Autolib.NFA.statemap
    alphamap = Autolib.NFA.alphamap
    complete = Autolib.NFA.Basic.sigmastar . setToList
    normalize = Autolib.NFA.Normalize.normalize
    trim     = Autolib.NFA.Trim.trim
    intersection a b = Autolib.NFA.Normalize.normalize
                     $ Autolib.NFA.Ops.intersection a b
    complement a = Autolib.NFA.Minus.complement (setToList $ letters a) a
