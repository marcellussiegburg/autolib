{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Autolib.Transport.HaXR (
    -- instances only
) where

import Data.Autolib.Transport.Atom
import Data.Autolib.Transport.Class

import qualified Data.Map as M
import qualified Data.Traversable as T
import qualified Data.ByteString as B
import Data.ByteString (ByteString)

import Network.XmlRpc.Internals

instance ConvertAtom Value Bool where
    fromAtom x = do ValueBool x' <- return x; return x'
    toAtom = ValueBool

instance ConvertAtom Value Double where
    fromAtom x = do ValueDouble x' <- return x; return x'
    toAtom = ValueDouble

-- FIXME: needs range checks
instance ConvertAtom Value Integer where
    fromAtom x = do ValueInt x' <- return x; return (toInteger x')
    toAtom = ValueInt . fromInteger

instance ConvertAtom Value String where
    fromAtom x = do ValueString x' <- return x; return x'
    toAtom = ValueString

instance ConvertAtom Value ByteString where
    fromAtom x = do
        ValueBase64 x' <- return x
        return $ B.pack . map (fromIntegral . fromEnum) $ x'
    toAtom = ValueBase64 . map (toEnum . fromIntegral) . B.unpack

instance Transport Value Value where
    encode (TrAtom x)    = x
    encode (TrArray xs)  = ValueArray  $ map encode xs
    encode (TrObject xs) = ValueStruct . M.assocs . M.map encode $ xs

    decode (ValueArray xs)  = TrArray `fmap` mapM decode xs
    decode (ValueStruct xs) = TrObject `fmap` T.mapM decode (M.fromList xs)
    decode x                = TrAtom `fmap` return x
