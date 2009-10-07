{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleContexts #-}

module Data.Autolib.Transport.Class (
    Transport(..),
    Trans(..),
    ToTransport(..)
) where

import Data.Autolib.Transport.Error
import Data.Autolib.Transport.Atom

data Trans atom
    = TrAtom atom
    | TrArray [Trans atom]
    | TrObject [(String, Trans atom)]

class Atom atom => Transport base atom | base -> atom where
    encode :: Trans atom -> base
    decode :: base -> Error (Trans atom)

class ToTransport a where
    toTransport :: Atom atom => a -> Trans atom
    fromTransport :: Atom atom => Trans atom -> Error a

    toTransportList :: Atom atom => [a] -> Trans atom
    fromTransportList :: Atom atom => Trans atom -> Error [a]

    toTransportList = toTransportList0
    fromTransportList = fromTransportList0

toTransportList0 :: (ToTransport a, Atom atom) => [a] -> Trans atom
toTransportList0 = TrArray . map toTransport

fromTransportList0 :: (ToTransport a, Atom atom) => Trans atom -> Error [a]
fromTransportList0 (TrArray xs) = mapM fromTransport xs
fromTransportList0 _ = fail "expected TrArray"
