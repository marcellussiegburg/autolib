module Rewriting.Path   where

--  $Id$

import Data.FiniteMap

import Reader
import ToDoc

class ( Show (FiniteMap v s)
      , Show (term v c)
      , Show s, Show v, Show c
      ) => PathC term v c s

instance ( Show (FiniteMap v s)
      , Show (term v c)
      , Show s, Show v, Show c
      ) => PathC term v c s

data Path term v c s = 
     Path { from :: s
	  , walk :: term v c
	  , to   :: FiniteMap v s
	  }
    deriving ( Show, Read )


instance PathC term v c s => 
    ToDoc ( Path term v c s ) where 
        toDoc = text . show

