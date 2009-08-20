-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Autolib.TES.Binu where

import Autolib.ToDoc
import Autolib.Symbol
import Autolib.Reader
-- import Text.XML.HaXml.Haskell2Xml
import Data.Typeable

-- | restricted case: binary symbol and nullary symbols
data ( ToDoc [c] , Reader [ c ] ) => Binu c = Binu
	  { binary  :: [c]
	  , unary   :: [c]
	  , nullary :: [c]
	  }
     deriving ( Typeable )

-- {-! for Binu derive: Haskell2Xml !-}
$(derives [makeReader, makeToDoc] [''Binu])

