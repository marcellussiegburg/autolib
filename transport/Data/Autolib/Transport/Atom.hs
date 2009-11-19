{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
  UndecidableInstances #-}

-- |
-- Conversion of atoms.

module Data.Autolib.Transport.Atom (
    ConvertAtom(..),
    Atom
) where

import Data.Autolib.Transport.Error
import Data.ByteString (ByteString)

-- |
-- The 'ConvertAtom' class is responsible for converting between atoms
-- and their underlying representation.
--
class ConvertAtom a b where
    fromAtom :: a -> Error b
    toAtom :: b -> a

-- |
-- 'Atom' is a class alias providing 'ConvertAtom' instances for 'Bool',
-- 'Double', 'Integer', 'String' and 'ByteString'.
--
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

