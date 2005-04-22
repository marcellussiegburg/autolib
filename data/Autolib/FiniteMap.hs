#define OLDFM 0

module Autolib.FiniteMap

( 
#if (OLDFM==1 || __GLASGOW_HASKELL__<604)
  module Data.FiniteMap
#else
  module Autolib.Data.Map
#endif
, module Autolib.FiniteMap
)

where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Set

#if (OLDFM==1 || __GLASGOW_HASKELL__<604)
import Data.FiniteMap
#else
import Autolib.Data.Map
#endif

import Autolib.Xml

import Data.Typeable

#if (OLDFM==1 || __GLASGOW_HASKELL__<604)
instance ( Ord a, Ord b ) => Ord ( FiniteMap a b ) where
    compare f g = compare (fmToList f) (fmToList g)
#endif

instance ( Typeable a, Typeable b ) => Typeable (FiniteMap a b) where
    typeOf (_ :: FiniteMap a b) = 
#if (__GLASGOW_HASKELL__ < 604)
	mkAppTy 
#else
        mkTyConApp
#endif
               (mkTyCon "FiniteMap") 
	       [ typeOf (undefined :: a), typeOf (undefined :: b) ]


instance (ToDoc a, ToDoc b) => ToDoc (FiniteMap a b)
    where toDocPrec p fm = 
	      docParen (p >= fcp) $ text "listToFM" <+> toDocPrec fcp (fmToList fm)


instance ( Ord a, Reader a, Reader b ) => Reader ( FiniteMap a b ) where
    atomic_readerPrec p = default_readerPrec p

default_readerPrec ::  ( Ord a, Reader a, Reader b ) 
	       => Int -> Parser ( FiniteMap a b )
default_readerPrec p = do
        guard ( p < 9 )
        my_reserved "listToFM"
	xys <-  reader
	return $ listToFM xys

------------------------------------------------------------------------

-- | specialized instances used for finite automata (testing)
instance  (ToDoc s, ToDoc c) => ToDoc (FiniteMap (s, c) (Set s)) where
    toDocPrec p fm = docParen (p >= fcp) 
		   $ text "collect" <+> toDocPrec fcp (unCollect fm)

instance  ( Ord c, Ord s, Reader s, Reader c, Reader [s] ) 
        => Reader (FiniteMap (s, c) (Set s)) where
    atomic_readerPrec p = default_readerPrec p <|> do 
        guard $ p < 9
        my_reserved "collect"
        xys <- reader
        return $ collect xys

-- | collect transition function from list of triples
collect :: ( Ord s, Ord c )
	=> [(s, c, s)] -> FiniteMap (s, c) (Set s)
collect pxqs = addListToFM_C union emptyFM $ do
    (p, x, q) <- pxqs
    return ((p, x), unitSet q)

-- | represent transition function as list of triples
unCollect :: FiniteMap (s, c) (Set s) -> [(s, c, s)]
unCollect fm = do
    ((p,c), qs) <- fmToList fm
    q <- setToList qs
    return (p, c, q)

-------------------------------------------------------------------------

instance (Ord a ) => Container (FiniteMap a b) [(a, b)] where
    label _ = "FiniteMap"
    pack = fmToList
    unpack = listToFM


mergeFM :: (Ord a, Ord b) => 
        FiniteMap a (Set b) -> FiniteMap a (Set b) -> FiniteMap a (Set b)
mergeFM l r = plusFM_C union l r

{-# INLINE lookupset #-}
lookupset :: Ord a => FiniteMap a (Set b) -> a -> Set b
lookupset fm x = case lookupFM fm x of
    Just m -> m; Nothing -> emptySet

