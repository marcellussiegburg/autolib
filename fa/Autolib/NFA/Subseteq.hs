{-# OPTIONS -fglasgow-exts #-}

module Autolib.NFA.Subseteq where

import Autolib.NFA.Type hiding ( subseteq )
import Autolib.NFA.Trim
import Autolib.NFA.Normalize
import Autolib.NFA.Minus
import Autolib.NFA.Shortest

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Multilingual

subsetequ :: (NFAC c a, NFAC c b, ToDoc [c])
	 => NFA c a -> NFA c b -> Reporter Bool
subsetequ a0 b0 = do
    inform $ fsep [ multitext [ (DE, "Ist")
			      , (UK, "Is")
			      ] ,  nest 4 $ info a0 
		  , multitext [ (DE, "Teilmenge von")
			      , (UK, "a subset of")
			      ],  nest 4 $ info b0
		  , text "?" 
		  ] 

    let a = trim $ normalize a0
	b = trim $ normalize b0

    let cut = 20 -- soviele testen
	dis = 5 -- soviele anzeigen

    let ein = -- einfach mal testen
	      filter ( not . is_accepted b ) 
                     $ take cut $ some_shortest a 
	zwei = -- richtig die automaten subtrahieren
	      some_shortest $ minus a b
	noh = if null ein then zwei else ein
	res = null noh

    if res 
       then inform $ multitext [ (DE, "Ja."), (UK, "Yes.") ]
       else inform $ fsep [ multitext [ (DE, "Nein. Wenigstens diese Wörter sind in" )
				      , (UK, "No. At least these words are in")
				      ]
			     ,  info a0
			     , comma
			     , multitext [ (DE, "aber nicht in")
					 , (UK, "but not in")
					 ]
			     ,  info b0, colon
			     ]
			$+$ nest 4 ( toDoc $ take dis noh )
    return res




