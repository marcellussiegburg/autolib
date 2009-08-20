-- -*- mode: haskell -*-

module Autolib.TES.Xml where

--  $Id$

import Text.XML.HaXml.Haskell2Xml

data XmlTerm = XmlTerm String

{-! for XmlTerm derive: Haskell2Xml !-}




