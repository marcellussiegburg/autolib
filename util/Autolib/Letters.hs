module Autolib.Util.Letters where

--   $Id$

import Data.Set

class Ord b => Letters a b | a -> b where
      letters :: a -> Set b


