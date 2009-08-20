-- -*- mode: haskell -*-

-- | long lists (i. e. they won't be clipped when showing)

module Autolib.Long where

--  $Id$

import Autolib.ToDoc
import Autolib.Reader

import Text.XML.HaXml.Haskell2Xml
import Data.Typeable

data Long a = Long { unLong :: [a] }
    deriving ( Eq, Ord, Typeable )

{-! for Long derive: Haskell2Xml !-}

instance Reader [a] => Reader (Long a) where
    atomic_reader = fmap Long reader

instance ToDoc a => ToDoc (Long a) where
    toDoc (Long xs) = unclipped_dutch_list $ map toDoc xs


instance Reader (Long Char) where
    atomic_reader = fmap Long reader

instance ToDoc (Long Char) where
    toDoc (Long cs) = text ( show cs ) -- should do the escaping






