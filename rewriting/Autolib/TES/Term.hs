module TES.Term 

( module TES.Type
, module Autolib.Symbol
, module TES.Apply
, module TES.Draw
)

where

--   $Id$

import Autolib.Symbol
import TES.Identifier

import TES.Type
import TES.In
import TES.Out
import TES.Apply
import TES.Draw

import TES.Xml
import Text.XML.HaXml.Haskell2Xml

instance ( Read (Term v c), TRSC v c ) 
    => Haskell2Xml (Term v c) where
    toContents t = toContents $ XmlTerm $ show t
    fromContents cs = 
        let ( XmlTerm s, rest ) = fromContents cs
	in  ( read s, rest )
    toHType (_ :: Term v c) = toHType ( undefined :: XmlTerm ) -- ??

