-- | fixme: wrong name (TES/Term confusion)

module Autolib.TES.Raw where

--  $Id$

import Data.Typeable

-- | TODO: include 'hash' data field in record (cached)
data Term v c = Node !c ![ Term v c ]
	      | Var  !v
     deriving ( Eq, Ord, Typeable )


