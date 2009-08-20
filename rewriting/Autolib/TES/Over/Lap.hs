-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Autolib.TES.Over.Lap where

import Autolib.Reader
import Autolib.ToDoc

import Autolib.TES.Term
import Autolib.TES.Position

data TRSC v c => Lap v c = 
     Lap { l :: Term v c
	     , r :: Term v c
	     , p :: Position
	     , s :: Substitution v c
	     }

$(derives [makeToDoc] [''Lap])

instance TRSC v c => Show ( Lap v c ) where
    show = render . toDoc

