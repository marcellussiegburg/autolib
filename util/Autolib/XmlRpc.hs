{-# LANGUAGE OverlappingInstances, IncoherentInstances, FlexibleContexts, UndecidableInstances #-}

module Autolib.XmlRpc ( ) where

import Autolib.Set 

import Network.XmlRpc.Internals

instance XmlRpcType Integer where
    getType _ = TStruct
    toValue x = toValue ( "integer", toValue $ show x )
    fromValue ( ValueStruct v ) = do
        s <- getField "integer" v
        ss <- fromValue s
        return $ read ss

instance XmlRpcType ( ) where
    toValue () = toValue ( [ ] :: [()] )
    fromValue v = return ()
    getType _ = TStruct


instance ( XmlRpcType a, XmlRpcType b ) =>  XmlRpcType ( a, b ) where
    toValue (a,b) = toValue [ ("first"  , toValue a )
                        , ("second" , toValue b )
                        ]
    fromValue ( ValueStruct v ) = do
                  f <- getField "first" v
                  ff <- fromValue f
                  s <- getField "second" v
                  ss <- fromValue s
                  return (ff, ss)
    getType _ = TStruct


instance ( Ord a, XmlRpcType [a] ) => XmlRpcType ( Set a ) where
    getType _ = TStruct
    toValue s = ValueStruct [("elements", toValue $ setToList s )]
    fromValue ( ValueStruct v ) = do
        xs <- getField "elements" v
        xss <- fromValue xs
        return $ mkSet xss

instance (XmlRpcType a, XmlRpcType b) => XmlRpcType (Either a b) where
    getType _ = TStruct
    toValue e = case e of
        Left  x -> ValueStruct [("tag", toValue False ), ("left" , toValue x)]
        Right y -> ValueStruct [("tag", toValue True ), ("right", toValue y)]
    fromValue ( ValueStruct v ) = do
        t <- getField "tag" v
        tt <- fromValue t
        case tt of
            False -> do x <- getField "left" v ; xx <- fromValue x ; return $ Left xx
            True  -> do x <- getField "right" v ; xx <- fromValue x ; return $ Right xx


