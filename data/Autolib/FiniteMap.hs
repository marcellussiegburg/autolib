module Autolib.FiniteMap

( module Data.FiniteMap
, module Autolib.FiniteMap
)

where

import Autolib.Reader
import Autolib.ToDoc


import Data.FiniteMap

-- import Text.XML.HaXml.Haskell2Xml
-- import Autolib.XmlFM
import Autolib.Xml

import Data.Typeable

instance ( Typeable a, Typeable b ) => Typeable (FiniteMap a b) where
    typeOf (_ :: FiniteMap a b) = 
	mkAppTy (mkTyCon "FiniteMap") 
	       [ typeOf (undefined :: a), typeOf (undefined :: b) ]


instance (ToDoc a, ToDoc b) => ToDoc (FiniteMap a b)
    where toDocPrec p fm = 
	      docParen (p >= fcp) $ text "listToFM" <+> toDocPrec fcp (fmToList fm)


instance ( Ord a, Reader a, Reader b ) => Reader ( FiniteMap a b ) where
    reader = do
        my_reserved "listToFM"
	xys <-  reader
	return $ listToFM xys


instance (Ord a ) => Container (FiniteMap a b) [(a, b)] where
    label _ = "FiniteMap"
    pack = fmToList
    unpack = listToFM



