module NFA.Eq where

-- $Id$

import NFA.Type hiding ( subseteq )

import NFA.Subseteq

-- import Boc
import Reporter
import ToDoc

{-
eq ::  (ToDoc [c], NFAC c a, NFAC c b)
	 => NFA c a -> NFA c b -> Boc
eq x y = cheporter $ equ x y
-}

equ :: (ToDoc [c], NFAC c a, NFAC c b)
	 => NFA c a -> NFA c b -> Reporter Bool
equ a b = do
    inform $ fsep [ text "Ist",  info a, text "gleich",  info b, text "?" ] 
    ab <- subsetequ a b
    ba <- subsetequ b a
    return $ ab && ba


