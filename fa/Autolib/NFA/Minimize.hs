module NFA.Minimize where

-- $Id$

import NFA.Type
import NFA.Dot
import NFA.Basic
import NFA.Det
import NFA.Mirror
import NFA.Normalize

minimize :: ( NFAC c Int, NFAC c (Set Int) ) => NFA c Int -> NFA c Int
minimize a = ( f $ f $ a ) { nfa_info = funni "minimize" [ info a ] }
    where f = normalize . det . mirror

-- der kleine unterschied

minimize0 :: ( NFAC c Int, NFAC c (Set Int) ) => NFA c Int -> NFA c Int
minimize0 a = ( f $ f $ a ) { nfa_info = funni "minimize" [ info a ] }
    where f = normalize . det0 . mirror




    

