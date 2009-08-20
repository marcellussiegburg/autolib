-- -*- mode: haskell -*-
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}

module Autolib.Graph.Kante where

--  $Id$

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Hash

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml
import Network.XmlRpc.Internals
import Network.XmlRpc.THDeriveXmlRpcType


data Kante a  = Kante
	      { von       :: a
	      , nach      :: a
	      } deriving (Eq, Ord, Typeable)

instance ( Ord a, XmlRpcType a ) =>  XmlRpcType ( Kante a ) where
    toValue k = toValue [ ("von"  , toValue $ von k )
                        , ("nach" , toValue $ nach k )
                        ]
    fromValue v = do
                  t <- fromValue v
                  f <- getField "von" t
                  s <- getField "nach" t
                  return $ kante f s
    getType _ = TStruct

instance Hash a => Hash (Kante a) where
    hash k = hash ( von k, nach k )

-- | ungerichteter Graph => Kanten von klein nach groß ordnen.
-- FIXME: und was ist für gerichtete Graphen?
kante :: Ord a => a -> a -> (Kante a)
kante x y = 
    if x < y then Kante { von = x, nach = y }
	     else Kante { von = y, nach = x }

-- {-! for Kante derive: Haskell2Xml !-}
      
instance ToDoc a => ToDoc (Kante a) where
    toDocPrec p k = docParen (p >= fcp) 
		  $ text "kante" <+> 
		    sep ( map ( toDocPrec fcp )  [ von k, nach k ] )

instance (Ord a, Reader a) => Reader (Kante a) where
    atomic_readerPrec d = do 
           guard $ d < 9
           my_reserved "kante"
	   v <- readerPrec 10
	   n <- readerPrec 10
	   return $ kante v n
    

