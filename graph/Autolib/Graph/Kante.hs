module Autolib.Graph.Kante where

-- -- $Id$

import Autolib.Reader
import Autolib.ToDoc


data Kante a  = Kante
	      { von       :: a
	      , nach      :: a
	      } deriving (Eq, Ord)

kante :: Ord a => a -> a -> (Kante a)
-- ungerichteter Graph => Kanten von klein nach groß ordnen.
kante x y = 
    if x < y then Kante { von = x, nach = y }
	     else Kante { von = y, nach = x }


      
instance ToDoc a => ToDoc (Kante a) where
    toDocPrec p k = docParen (p >= fcp) 
		  $ text "kante" <+> 
		    sep ( map ( toDocPrec fcp )  [ von k, nach k ] )

instance (ToDoc a) => Show (Kante a) where
  show = render . toDoc


instance (Ord a, Reader a) => Reader (Kante a) where
    readerPrec d = readerParen (d > 9) 
      $ do my_reserved "kante"
	   v <- readerPrec 10
	   n <- readerPrec 10
	   return $ kante v n
    
instance  (Ord a, Reader a ) => Read ( Kante a ) where
     readsPrec = parsec_readsPrec

