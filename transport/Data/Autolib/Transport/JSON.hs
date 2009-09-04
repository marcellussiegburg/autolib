{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Autolib.Transport.JSON (
    -- instances only
) where

import Data.Autolib.Transport.Atom
import Data.Autolib.Transport.Class

import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified Data.ByteString as B
import Data.ByteString (ByteString)

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
    encode (Atom x)    = x
    encode (Array xs)  = JSArray $ map encode xs
    encode (Object xs) = JSObject . toJSObject . M.assocs . M.map encode $ xs

    decode (JSArray xs) = Array `fmap` mapM decode xs
    decode (JSObject o) = Object `fmap` T.mapM decode (M.fromList . fromJSObject $ o)
    decode x            = return (Atom x)
