module NFA.Minimize where

-- -- $Id$

import NFA.Type
import NFA.Dot
import NFA.Basic
import NFA.Det
import NFA.Mirror
import NFA.Trim
import NFA.Normalize

import Reporter
import ToDoc
import Util.Size

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




    

