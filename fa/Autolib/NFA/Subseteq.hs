module Autolib.NFA.Subseteq where

-- -- $Id$

import Autolib.NFA.Type hiding ( subseteq )
import Autolib.NFA.Trim
import Autolib.NFA.Normalize
import Autolib.NFA.Minus
import Autolib.NFA.Shortest

import Autolib.Reporter
import Autolib.ToDoc

subsetequ :: (NFAC c a, NFAC c b, ToDoc [c])
	 => NFA c a -> NFA c b -> Reporter Bool
subsetequ a0 b0 = do
    inform $ fsep [ text "Ist",  nest 4 $ info a0 
		  , text "Teilmenge von",  nest 4 $ info b0
		  , text "?" 
		  ] 

    let a = trim $ normalize a0
	b = trim $ normalize b0

    let cut = 20 -- soviele testen
	dis = 5 -- soviele anzeigen

    let ein = -- einfach mal testen
	      filter ( not . is_accepted b ) $ take cut $ accepted a
	zwei = -- richtig die automaten subtrahieren
	      accepted $ minus a b
	noh = if null ein then zwei else ein
	res = null noh

    if res 
       then inform $ text "Ja."
       else inform $ fsep [ text "Nein. Wenigstens diese Wörter sind in"
			     ,  info a0
			     , comma, text "aber nicht in"
			     ,  info b0, colon
			     ]
			$+$ nest 4 ( toDoc $ take dis noh )
    return res




