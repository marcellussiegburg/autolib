module Virtualist where

--  $Id$

import Reader
import ToDoc
import Letters
import Sets

newtype Virtualist v a = Virtualist { unVirtualist :: [ a ] }
    deriving ( Eq, Ord )

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



