module Letters where

-- -- $Id$

import Set

class Letters a b where
      letters :: a -> Set b

