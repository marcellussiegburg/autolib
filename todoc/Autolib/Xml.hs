{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}

module Autolib.Xml 

( Container (..)
)

where

--  $Id$

class Container a con | a -> con  where
    label  :: a -> String
    pack   :: a -> con
    unpack :: con -> a
