{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- HaXR backend.
--
-- This module provides 'Transport' and 'Atom' instances for HaXR.

module Data.Autolib.Transport.HaXR (
    -- instances only
) where

import Data.Autolib.Transport.Atom
import Data.Autolib.Transport.Class

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Control.Arrow (second)

import qualified Data.String.UTF8 as U
import Data.Word

import Network.XmlRpc.Internals

instance ConvertAtom Value Bool where
    fromAtom x = do ValueBool x' <- return x; return x'
    toAtom = ValueBool

instance ConvertAtom Value Double where
    fromAtom x = do ValueDouble x' <- return x; return x'
    toAtom = ValueDouble

-- Note: We encode integers as strings because XMLRPC limits Ints to 32 bits.
instance ConvertAtom Value Integer where
    fromAtom x = do ValueString x' <- return x; return (read x')
    toAtom = ValueString . show

instance ConvertAtom Value String where
    fromAtom x = do ValueString x' <- return x; return x' -- (du x')
    toAtom = ValueString . eu

du :: String -> String
du = U.toString . U.fromRep . map (fromIntegral :: Int -> Word8) . map fromEnum

eu :: String -> String
eu = map toEnum . map (fromIntegral :: Word8 -> Int) . U.toRep . U.fromString 

instance ConvertAtom Value ByteString where
    fromAtom x = do
        ValueBase64 x' <- return x
        return $ B.pack . map (fromIntegral . fromEnum) $ x'
    toAtom = ValueBase64 . map (toEnum . fromIntegral) . B.unpack

instance Transport Value Value where
    encode (TrAtom x)    = x
    encode (TrArray xs)  = ValueArray  $ map encode xs
    encode (TrObject xs) = ValueStruct . map (second encode) $ xs

    decode (ValueArray xs)  = TrArray `fmap` mapM decode xs
    decode (ValueStruct xs) = TrObject `fmap` mapM (secondM decode) xs
    decode x                = TrAtom `fmap` return x

secondM :: Monad m => (a -> m b) -> (x, a) -> m (x, b)
secondM f (x, a) = f a >>= return . (,) x
