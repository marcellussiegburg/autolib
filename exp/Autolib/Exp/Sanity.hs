-- $Id$

module Exp.Sanity where


import Exp.Env
import Exp.Type
import Exp.Syntax

import Report
import Set
import FAU
import Wrong

sanity :: Set Char -> Set String -> Exp -> Report ()
sanity alpha keys x = do
    let subs = subtrees x

    let letters = mkSet [ c | Letter c <- subs ]
    let strange = letters `minusSet` alpha
    assert ( isEmptySet strange )
	   $ "Ausdruck enthält unbekannte Nichtterminale: " ++ show strange
	   ++ ", zugelassen sind nur: " ++ show alpha
    
    let refs = mkSet [ n | Ref n <- subs ]
    let strange = refs `minusSet` keys
    assert ( isEmptySet strange )   
	   $ "Ausdruck enthält unbekannte Bezeichner: " ++ show strange
	   ++ ", zugelassen sind nur: " ++ show keys

sane alpha keys x cont = 
    case sanity alpha keys x of
	 Fail msg -> do putStrLn msg; wrong
	 OK _   -> cont
