module Virtualist 

( Virtualist (..)
, virtualist
)

where

--  $Id$

import Reader
import ToDoc
import Letters
import Sets
import Hash
import Size

data Virtualist v a = Virtualist 
		    { unVirtualist :: [ a ] 
		    , plug :: v
		    }
    deriving ( Eq, Ord )

virtualist v xs = Virtualist { unVirtualist = xs, plug = v }

instance Functor ( Virtualist v ) where
    fmap f xs = Virtualist { unVirtualist = map f $ unVirtualist xs
			   , plug = plug xs
			   }

instance ToDoc [a] => ToDoc ( Virtualist v a ) where
    toDoc = toDoc . unVirtualist

instance Reader [a] => Reader ( Virtualist v a ) where
    reader = fmap ( \ xs -> Virtualist { unVirtualist = xs 
				       , plug = undefined
				       }
		  ) reader

instance ToDoc [a] => Show ( Virtualist v a ) where
    show = render . toDoc

instance Reader [a] => Read ( Virtualist v a ) where
    readsPrec = parsec_readsPrec

instance Ord a => Letters (Virtualist v a) a where
    letters = mkSet . unVirtualist

instance ( Eq a, Eq v, Hash [a] ) => Hash (Virtualist v a) where
    hash = hash . unVirtualist

instance Size [a] => Size (Virtualist v a) where
    size = size . unVirtualist

