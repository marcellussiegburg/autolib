module Autolib.NFA.Minimize where

-- -- $Id$

import Autolib.NFA.Type
import Autolib.NFA.Dot
import Autolib.NFA.Basic
import Autolib.NFA.Det
import Autolib.NFA.Mirror
import Autolib.NFA.Trim
import Autolib.NFA.Normalize

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Util.Size

minimizeR :: ( NFAC c Int, NFAC c (Set Int) ) 
	  => NFA c Int -> Reporter ( NFA c Int )
minimizeR a = do
    inform $ text "minimizeR"
    let f a = do
	  let b = mirror a
	      c = det b
	      d = normalize c
	      e = trim d
	  inform $ vcat 
		 [ text "f.input" <+> toDoc (size a)
		 , text "f.mirror" <+> toDoc (size b)
		 , text "f.det" <+> toDoc (size c)
		 , text "f.normalize" <+> toDoc (size d)
		 , text "f.trim" <+> toDoc (size e)
		 ]
	  return e
    b <- f a
    c <- f b
    return $ c { nfa_info = funni "minimizeR" [ info a ] }
	  

minimize :: ( NFAC c Int, NFAC c (Set Int) ) 
	 => NFA c Int -> NFA c Int
minimize a = ( f $ f $ a ) { nfa_info = funni "minimize" [ info a ] }
    where f = normalize . det . mirror

-- der kleine unterschied

minimize0 :: ( NFAC c Int, NFAC c (Set Int) ) => NFA c Int -> NFA c Int
minimize0 a = ( f $ f $ a ) { nfa_info = funni "minimize" [ info a ] }
    where f = normalize . det0 . mirror




    

