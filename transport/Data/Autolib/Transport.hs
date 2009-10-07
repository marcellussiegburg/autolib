{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Autolib.Transport (
    module Data.Autolib.Transport.Class,
    module Data.Autolib.Transport.Atom,
    module Data.Autolib.Transport.Error,
    module Data.Derive.ToTransport
) where

import Data.Autolib.Transport.Class
import Data.Autolib.Transport.Atom
import Data.Autolib.Transport.Error
import Data.Autolib.Transport.JSON ()
import Data.Autolib.Transport.HaXR ()
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

instance ToTransport a => ToTransport [a] where
    toTransport = TrArray . map toTransport
    fromTransport (TrArray xs) = mapM fromTransport xs
    fromTransport _ = fail "expected TrArray"

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

instance ToTransport String where
    toTransport = TrAtom . toAtom
    fromTransport (TrAtom x) = fromAtom x
    fromTransport _ = fail "expected TrAtom"

instance ToTransport ByteString where
    toTransport = TrAtom . toAtom
    fromTransport (TrAtom x) = fromAtom x
    fromTransport _ = fail "expected TrAtom"

instance ToTransport Bool where
    toTransport = TrAtom . toAtom
    fromTransport (TrAtom x) = fromAtom x
    fromTransport _ = fail "expected TrAtom"

