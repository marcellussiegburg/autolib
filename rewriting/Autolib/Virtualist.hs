module Virtualist where

--  $Id$

import Reader
import ToDoc
import Letters
import Sets
import Hash
import Size

newtype Virtualist v a = Virtualist { unVirtualist :: [ a ] }
    deriving ( Eq, Ord )

instance Functor ( Virtualist v ) where
    fmap f xs = Virtualist $ map f $ unVirtualist xs

instance ToDoc [a] => ToDoc ( Virtualist v a ) where
    toDoc = toDoc . unVirtualist

instance Reader [a] => Reader ( Virtualist v a ) where
    reader = fmap Virtualist reader

instance ToDoc [a] => Show ( Virtualist v a ) where
    show = render . toDoc

instance Reader [a] => Read ( Virtualist v a ) where
    readsPrec = parsec_readsPrec

instance Ord a => Letters (Virtualist v a) a where
    letters = mkSet . unVirtualist

instance Hash [a] => Hash (Virtualist v a) where
    hash = hash . unVirtualist

instance Size [a] => Size (Virtualist v a) where
    size = size . unVirtualist

