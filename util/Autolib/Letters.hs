module Letters where

-- $Id$

import Set

class Letters a where
      letters :: a -> Set Char

