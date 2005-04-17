module Autolib.TES.Inverse where

--  $Id$

import Autolib.TES.Data
import Autolib.TES.Rule

inverse :: TRS v c -> TRS v c
inverse trs = trs { rules = do 
	r <- rules trs
	return $ r { lhs = rhs r , rhs = lhs r }
    }

