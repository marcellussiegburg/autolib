{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- 'Transport' abstraction layer.
--
-- This library provides a common interface for representing data as
-- JSON or XML-Rpc data. The common structure is captured in the 'Trans'
-- type.
--
-- To use it you'll need one of the backend implementations,
-- 'Data.Autolib.Transport.JSON' or 'Data.Autolib.Transport.HaXR'.

module Data.Autolib.Transport (
    module Data.Autolib.Transport.Class,
    module Data.Autolib.Transport.Atom,
    module Data.Autolib.Transport.Error,
    module Data.Derive.ToTransport
    -- and lots of instances
) where

import Data.Autolib.Transport.Class
import Data.Autolib.Transport.Atom
import Data.Autolib.Transport.Error
import Data.Derive.ToTransport (makeToTransport, derives)

import Data.ByteString (ByteString)
import qualified Data.Set as S
import qualified Data.Map as M

instance ToTransport () where
    toTransport   () = TrArray []
    fromTransport (TrArray []) = return ()
    fromTransport _ = fail "expected empty TrArray"

instance (ToTransport a, ToTransport b) => ToTransport (a, b) where
    toTransport (a, b) = TrArray [toTransport a, toTransport b]
    fromTransport (TrArray [a, b]) = do
        a' <- fromTransport a
        b' <- fromTransport b
        return (a', b')
    fromTransport _ = fail "expected two-element TrArray"

instance (ToTransport a, ToTransport b, ToTransport c)
    => ToTransport (a, b, c) where
    toTransport (a, b, c)
        = TrArray [toTransport a, toTransport b, toTransport c]
    fromTransport (TrArray [a, b, c]) = do
        a' <- fromTransport a
        b' <- fromTransport b
        c' <- fromTransport c
        return (a', b', c')
    fromTransport _ = fail "expected three-element TrArray"

instance ToTransport a => ToTransport [a] where
    toTransport = toTransportList
    fromTransport = fromTransportList

instance (Ord a, ToTransport a) => ToTransport (S.Set a) where
    toTransport = TrArray . map toTransport . S.toList
    fromTransport (TrArray xs) = S.fromList `fmap` mapM fromTransport xs
    fromTransport _ = fail "expected TrArray"

instance (Ord a, ToTransport a, ToTransport b) => ToTransport (M.Map a b) where
    toTransport = TrArray . map toTransport . M.toList
    fromTransport (TrArray xs) = M.fromList `fmap` mapM fromTransport xs
    fromTransport _ = fail "expected TrArray"

instance (ToTransport a, ToTransport b) => ToTransport (Either a b) where
    toTransport (Left  a) = TrObject [("Left",  toTransport a)]
    toTransport (Right b) = TrObject [("Right", toTransport b)]
    fromTransport (TrObject [("Left",  a)]) = Left  `fmap` fromTransport a
    fromTransport (TrObject [("Right", b)]) = Right `fmap` fromTransport b
    fromTransport _ = fail "expected TrObject with a Left or a Right field"

instance ToTransport Int where
    toTransport = toTransport . toInteger
    fromTransport x = fromInteger `fmap` fromTransport x

-- instances for atoms
instance ToTransport Integer where
    toTransport = TrAtom . toAtom
    fromTransport (TrAtom x) = fromAtom x
    fromTransport _ = fail "expected TrAtom"

instance ToTransport Double where
    toTransport = TrAtom . toAtom
    fromTransport (TrAtom x) = fromAtom x
    fromTransport _ = fail "expected TrAtom"

instance ToTransport Char where
    toTransport = toTransportList . (:[])
    fromTransport x = do
        [c] <- fromTransportList x
        return c

    toTransportList = TrAtom . toAtom
    fromTransportList (TrAtom x) = fromAtom x
    fromTransportList _ = fail "expected TrAtom"

instance ToTransport ByteString where
    toTransport = TrAtom . toAtom
    fromTransport (TrAtom x) = fromAtom x
    fromTransport _ = fail "expected TrAtom"

instance ToTransport Bool where
    toTransport = TrAtom . toAtom
    fromTransport (TrAtom x) = fromAtom x
    fromTransport _ = fail "expected TrAtom"

