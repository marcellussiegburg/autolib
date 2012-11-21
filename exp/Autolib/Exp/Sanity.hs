module Autolib.Exp.Sanity where

import Autolib.Exp.Env
import Autolib.Exp.Type
import Autolib.Exp.Syntax

import Autolib.Reporter
import Autolib.Set
import Autolib.ToDoc


sanity :: ( Ord c, ToDoc [c] )
       => Set c
       -> Set String 
       -> RX c 
       -> Reporter ()
sanity alpha keys x = do
    sanity_alpha alpha x
    sanity_keys  keys  x

sanity_alpha :: ( Ord c, ToDoc [c] )
       => Set c
       -> RX c 
       -> Reporter ()
sanity_alpha alpha x = do
    let subs = subtrees x
    let letters = mkSet [ c | Letter c <- subs ]
    let strange = letters `minusSet` alpha
    when  ( not $ isEmptySet strange )
	   $ reject $ vcat
	   [ text "Ausdruck enthält unbekannte Buchstaben:"
	   , nest 4 $ toDoc strange
	   , text "zugelassen sind nur:"
	   , nest 4 $ toDoc alpha
	   ]
    
sanity_keys :: ( Ord c, ToDoc [c] )
       => Set String
       -> RX c 
       -> Reporter ()
sanity_keys keys x = do
    let subs = subtrees x
    let refs = mkSet [ n | Ref n <- subs ]
    let strange = refs `minusSet` keys
    when ( not $ isEmptySet strange )   
	   $ reject $ vcat
	   [ text "Ausdruck enthält unbekannte Bezeichner:"
	   , nest 4 $ toDoc strange
	   , text "zugelassen sind nur:"
	   , nest 4 $ toDoc keys
	   ]

