{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

module Autolib.TES.Term 

( module Autolib.TES.Type
, module Autolib.Symbol
, module Autolib.TES.Apply
, module Autolib.TES.Draw
)

where

--   $Id$

import Autolib.Symbol
import Autolib.TES.Identifier

import Autolib.TES.Type
import Autolib.TES.In
import Autolib.TES.Out
import Autolib.TES.Apply
import Autolib.TES.Draw

import Autolib.TES.Xml
import Text.XML.HaXml.Haskell2Xml

instance ( Read (Term v c), TRSC v c ) 
    => Haskell2Xml (Term v c) where
    toContents t = toContents $ XmlTerm $ show t
    fromContents cs = 
        let ( XmlTerm s, rest ) = fromContents cs
	in  ( read s, rest )
    toHType (_ :: Term v c) = toHType ( undefined :: XmlTerm ) -- ??

