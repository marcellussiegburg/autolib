{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Autolib.Transport (
    module Data.Autolib.Transport.Class,
    module Data.Autolib.Transport.Atom,
    module Data.Autolib.Transport.Error
) where

import Data.Autolib.Transport.Class
import Data.Autolib.Transport.Atom
import Data.Autolib.Transport.Error
import Data.Autolib.Transport.JSON ()
import Data.Autolib.Transport.HaXR ()

import Data.ByteString (ByteString)

instance ToTransport () where
    toTransport   () = TrArray []
    fromTransport (TrArray []) = return ()
    fromTransport _ = fail "expected empty TrArray"

instance ToTransport a => ToTransport [a] where
    toTransport = TrArray . map toTransport
    fromTransport (TrArray xs) = mapM fromTransport xs
    fromTransport _ = fail "expected TrArray"

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

