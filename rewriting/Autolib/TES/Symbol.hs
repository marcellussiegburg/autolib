module TES.Symbol where

--   $Id$

import ToDoc
import Reader

import SRS.Aged

class ( Eq s, Ord s, ToDoc s, Reader s ) => Symbol s where
    arity :: s -> Int

instance ( Show a, Symbol a ) => Symbol (Aged a) where
    arity = arity . it


    
