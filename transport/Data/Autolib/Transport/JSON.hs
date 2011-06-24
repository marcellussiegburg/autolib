{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- JSON backend
--
-- This module provides 'Transport' and 'Atom' instances for JSON.

module Data.Autolib.Transport.JSON (
    -- instances only
) where

import Data.Autolib.Transport.Atom
import Data.Autolib.Transport.Class

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Control.Arrow (second)

import Text.JSON (
    JSValue (..), fromJSString, toJSString, fromJSObject, toJSObject)
import qualified Codec.Binary.Base64.String as C

-- bools are supported directly
instance ConvertAtom JSValue Bool where
    fromAtom x = do JSBool x' <- return x; return x'
    toAtom = JSBool

-- doubles are stored as numbers (which Text.JSON stores a bit oddly)
instance ConvertAtom JSValue Double where
    fromAtom x = do JSRational _ x' <- return x; return (fromRational x')
    toAtom = JSRational False . toRational

-- integers are stored as numbers as well
instance ConvertAtom JSValue Integer where
    fromAtom x = do JSRational _ x' <- return x; return (truncate x')
    toAtom = JSRational False . fromInteger

-- strings as strings
instance ConvertAtom JSValue String where
    fromAtom x = do JSString x' <- return x; return (fromJSString x')
    toAtom = JSString . toJSString

-- and binaries as base64-encoded strings
instance ConvertAtom JSValue ByteString where
    fromAtom x = do
        JSString x' <- return x
        return $ B.pack . map (fromIntegral . fromEnum)
               . C.decode . fromJSString $ x'
    toAtom = JSString . toJSString . C.encode
           . map (toEnum . fromIntegral) . B.unpack

instance Transport JSValue JSValue where
    encode (TrAtom x)    = x
    encode (TrArray xs)  = JSArray $ map encode xs
    encode (TrObject xs) = JSObject . toJSObject . map (second encode) $ xs

    decode (JSArray xs) = TrArray `fmap` mapM decode xs
    decode (JSObject o) = TrObject `fmap` mapM (secondM decode) (fromJSObject o)
    decode x            = TrAtom `fmap` return x

secondM :: Monad m => (a -> m b) -> (x, a) -> m (x, b)
secondM f (x, a) = f a >>= return . (,) x
