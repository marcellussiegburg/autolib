module Letters where

--   $Id$

import Data.Set

class Letters a b where
      letters :: a -> Set b

