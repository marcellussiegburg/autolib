-- $Header$

-- das geh�rt jetzt eigentlich in  fa/

module Vergleiche where

import NFA
import Minus ( minus )
import Inter
import ToDoc

import Wrong
import Right

vergleiche :: String -> NFA Int -> NFA Int -> IO String
vergleiche ok soll ist = 
    vergleiche_with_msg minus ok ("L(A)", soll) ("L(S)", ist)

vergleiche_det :: String -> NFA Int -> NFA Int -> IO String
vergleiche_det ok soll ist = 
    vergleiche_with_msg minus_det ok ("L(A)", soll) ("L(S)", ist)

type Sub = NFA Int -> NFA Int -> NFA Int

vergleiche_with_msg 
    :: Sub -> String -> (String, NFA Int) -> (String, NFA Int) -> IO String
vergleiche_with_msg sub ok (sname, soll) (iname, ist) = do
    
    putStrLn $ "\nEinige W�rter der Sprache " ++ sname ++ ":" 
    putz $ accepted soll

    putStrLn $ "\nEinige W�rter der Sprache " ++ iname ++ ":" 
    putz $ accepted ist
    
    let smi = soll `sub` ist
    let asmi = accepted smi
    let flag_smi = null asmi

    let ims = ist `sub` soll 
    let aims = accepted ims
    let flag_ims = null aims

    let nl = "\n"

    if flag_ims && flag_smi
       then do putStrLn $ unwords 
	           [ nl, sname, "und", iname ,"stimmen �berein." ]
	       right_with ok
       else do putStrLn $ unwords 
	           [ nl, sname, "und", iname ,"stimmen nicht �berein." ]
	       putStrLn $ unwords
	    	   [ nl, sname, "\\",  iname
		   , "enth�lt unter anderem diese W�rter:" , nl
		   ]
	       putz $ asmi

	       putStrLn $ unwords 
	           [ nl, iname, "\\",  sname
		   , "enth�lt unter anderem diese W�rter:" , nl
		   ]
	       putz $ aims
	       wrong

    


