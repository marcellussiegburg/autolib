module Autolib.Exp.Einfach where

-- -- $Id$

import Autolib.Exp.Type
import Autolib.Exp.Syntax

import Autolib.Set
import Autolib.Reporter
import Autolib.ToDoc

check_form :: Set String -> Exp -> Reporter ()
check_form ss x = do
    let cs = constructors x
	ds = minusSet cs ss
	t = vcat [ text "Es sind nur diese Operatoren zugelassen:"
		 , nest 4 $ toDoc ss
		 , text "und diese Operatoren kommen vor:"
		 , nest 4 $ toDoc cs
		 ]
    if isEmptySet ds
       then inform $ text "Alle Operatoren sind erlaubt."
       else reject $ text "Diese Operatoren sind nicht erlaubt:" 
		      <+> toDoc ds

ist_einfach = check_form 
	    $ mkSet [ "Ref", "Letter" , "Dot", "Union", "Star" ]
ist_erweitert = check_form 
	      $ mkSet [ "Ref", "Letter" , "Dot"
		      , "Union", "Intersection", "Difference", "Star" 
		      ]

{-
einfach :: Exp -> IO String -> IO String
einfach x = moc ( ist_einfach x )

erweitert :: Exp -> IO String -> IO String
erweitert x = moc ( ist_erweitert x )
-}

