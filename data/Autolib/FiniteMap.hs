module Autolib.FiniteMap

( module Data.FiniteMap
, module Autolib.FiniteMap
)

where

import Autolib.XmlFM

import Data.FiniteMap
import Text.XML.HaXml.Haskell2Xml

instance (Ord a, Haskell2Xml a, Haskell2Xml b) => Haskell2Xml (FiniteMap a b) where
    toContents s = toContents $ XmlFM $ fmToList s 
    fromContents cs = 
        let ( XmlFM x, rest ) = fromContents cs
	in  ( listToFM x, rest )
    toHType (_ :: FiniteMap a b) = toHType (undefined :: XmlFM a b) -- ??

