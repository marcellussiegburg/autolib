{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleContexts #-}

-- |
-- 'Atom' and 'ToTransport' classes.

module Data.Autolib.Transport.Class (
    Transport(..),
    Trans(..),
    ToTransport(..)
) where

import Data.Autolib.Transport.Error
import Data.Autolib.Transport.Atom

-- |
-- Intermediate data representation. It can contain atoms (Bool, Double,
-- Integer, String, ByteString), arrays (lists) and objects (maps with
-- string keys).
--
data Trans atom
    = TrAtom atom
    | TrArray [Trans atom]
    | TrObject [(String, Trans atom)]

-- |
-- Final conversion to transport layer.
--
class Atom atom => Transport base atom | base -> atom where
    encode :: Trans atom -> base
    decode :: base -> Error (Trans atom)

-- |
-- Conversion between data and intermediate representation.
--
class ToTransport a where
    -- | Convert data to intermediate representation.
    toTransport :: Atom atom => a -> Trans atom
    -- | Convert intermediate representation to data.
    fromTransport :: Atom atom => Trans atom -> Error a

    -- | This is analoguous to 'showList', used for the 'String' instance.
    toTransportList :: Atom atom => [a] -> Trans atom
    -- | This is analoguous to 'readList', used for the 'String' instance.
    fromTransportList :: Atom atom => Trans atom -> Error [a]

    toTransportList = toTransportList0
    fromTransportList = fromTransportList0

toTransportList0 :: (ToTransport a, Atom atom) => [a] -> Trans atom
toTransportList0 = TrArray . map toTransport

fromTransportList0 :: (ToTransport a, Atom atom) => Trans atom -> Error [a]
fromTransportList0 (TrArray xs) = mapM fromTransport xs
fromTransportList0 _ = fail "expected TrArray"
