module Autolib.Rewriting.Path   

( Path
, module Autolib.Rewriting.Path.Data
)

where

--  $Id$

import Data.FiniteMap

import Autolib.Rewriting.Path.Data 

import Autolib.Reader
import Autolib.ToDoc

class ( Show (FiniteMap v s)
      , Show (term v c)
      , Show s, Show v, Show c
      ) => PathC term v c s

instance ( Show (FiniteMap v s)
      , Show (term v c)
      , Show s, Show v, Show c
      ) => PathC term v c s

type Path term v c s = Data s ( term v c ) ( FiniteMap v s )

{-
data Path term v c s = 
     Path { from :: s
	  , walk :: term v c
	  , to   :: FiniteMap v s
	  }
    deriving ( Show, Read )

instance PathC term v c s => 
    ToDoc ( Path term v c s ) where 
        toDoc = text . show
-}





