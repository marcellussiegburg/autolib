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

instance ( Haskell2Xml v, Haskell2Xml c ) 
         => Haskell2Xml ( Term v c ) where
    toContents ( Var v ) = return $ mkel "var" $ toContents v 
    toContents ( Node f xs ) = return $ mkel "app"
         $ mkel "fun" ( toContents f )
         : map ( \ x -> mkel "arg" $ toContents x ) xs

mkel name cs = CElem $ Elem name [] cs
