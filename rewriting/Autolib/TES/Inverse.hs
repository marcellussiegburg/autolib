module Autolib.TES.Inverse where

--  $Id$

import Autolib.TES.Data

inverse :: TRS v c -> TRS v c
inverse trs = trs { rules = do ( l, r ) <- rules trs ; return ( r, l ) }
