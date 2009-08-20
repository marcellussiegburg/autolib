{-# LANGUAGE TemplateHaskell, OverlappingInstances #-}

module Autolib.ToDoc.Derived where

--  $Id$

import Autolib.ToDoc.Class
import Autolib.ToDoc.Dutch
import Data.Derive.ToDoc

$(derives [makeToDoc] [''Bool, ''Maybe, ''Either])

-- Local Variables: 
-- mode: haskell
-- End:


