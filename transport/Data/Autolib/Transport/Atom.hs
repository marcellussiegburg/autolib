{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Data.Autolib.Transport.Atom (
    ConvertAtom(..),
    Atom
) where

import Data.ByteString (ByteString)

class ConvertAtom a b where
    fromAtom :: a -> Maybe b
    toAtom :: b -> a

class (
    ConvertAtom a Bool,
    ConvertAtom a Double,
    ConvertAtom a Integer,
    ConvertAtom a String,
    ConvertAtom a ByteString
 ) => Atom a

instance (
    ConvertAtom a Bool,
    ConvertAtom a Double,
    ConvertAtom a Integer,
    ConvertAtom a String,
    ConvertAtom a ByteString
 ) => Atom a

