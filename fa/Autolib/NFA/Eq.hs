module Autolib.NFA.Eq where

--  $Id$

import Autolib.NFA.Type hiding ( subseteq )

import Autolib.NFA.Subseteq

import Autolib.Reporter
import Autolib.ToDoc

equ :: (ToDoc [c], NFAC c a, NFAC c b)
	 => NFA c a -> NFA c b -> Reporter Bool
equ a b = do
    inform $ fsep [ text "Ist",  nest 4 $ info a
		  , text "gleich",  nest 4 $  info b
		  , text "?" ] 
    ab <- nested 4 $ subsetequ a b
    ba <- nested 4 $ subsetequ b a
    return $ ab && ba


