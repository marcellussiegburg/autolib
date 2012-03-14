{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}

--   $Id$

module Autolib.NFA.Data

( module Autolib.NFA.Data
, module Autolib.Informed
, module Autolib.FiniteMap
, module Autolib.Set
)

where

import Autolib.Set
import Autolib.Size
import Autolib.Letters

import Autolib.FiniteMap

import Autolib.Informed
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash

import Autolib.Symbol

import Control.Monad (guard)
import Data.Typeable

-- mal sehen
type NFA_Char s = NFA Char s

class ( Ord c, Ord s
      , Hash c, Hash s
      , Symbol c
      , ToDoc [c]
      , ToDoc s, ToDoc [s]
      , Show s -- ??
      , Reader [c]
      , Reader s, Reader [s]
	        
      ) => NFAC c s 
-- ohne methoden, nur constraints sammeln


instance ( Ord c, Ord s
	 , Symbol c
	 , Hash c, Hash s
      , ToDoc [c]
      , ToDoc s, ToDoc [s]
      , Show s -- ??
      , Reader [c]
      , Reader s, Reader [s]
      ) => NFAC c s


data NFAC c s => NFA c s = 
     NFA { nfa_info   :: Doc   -- wird nicht gelesen und auch nicht geprintet
	 , alphabet :: Set c
	 , states :: Set s
	 , starts :: Set s
	 , finals :: Set s
	 , trans  :: FiniteMap (s, c) (Set s)
	 }
    deriving Typeable

$(derives [makeToDoc, makeReader] [''NFA])

instance NFAC c s => Show (NFA c s) where show = render . toDoc

instance NFAC c s
    => Informed (NFA c s) where
    info = nfa_info
    informed i a = a { nfa_info = i }

instance NFAC c s
    => Size (NFA c s) where 
    size = cardinality . states

instance NFAC c s 
    => Hash ( NFA c s ) where
    hash  = hash . essence

instance NFAC c s
    => Eq ( NFA c s ) where
    a == b  =  essence a == essence b

essence a = ( states a, starts a, finals a, trans a )

-- local variables:
-- mode: haskell
-- end
