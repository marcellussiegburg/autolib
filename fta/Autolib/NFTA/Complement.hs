module Autolib.NFTA.Complement where

--  $Id$

import Autolib.NFTA.Type
import Autolib.NFTA.Det
import Autolib.NFTA.Trim
import Autolib.NFTA.Normalize

-- | must be complete (as produced by det, for instance)
com a = a { finals = states a `minusSet` finals a }
    
complement :: NFTAC c s 
	   => NFTA c s -> NFTA c Int
complement = trim . normalize . com . det

