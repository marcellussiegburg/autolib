module Exp.Einfach where

-- $Id$

import Exp.Type
import Exp.Syntax

import Set
import Boc


check_form :: Set String -> Exp -> Boc
check_form ss x = 
    let cs = constructors x
	ds = minusSet cs ss
	t = vcat [ text "Es sind nur diese Operatoren zugelassen:"
		 , nest 4 $ toDoc ss
		 , text "und diese Operatoren kommen vor:"
		 , nest 4 $ toDoc cs
		 ]
    in	explain 0 t
	$ if isEmptySet ds
	  then ( True, text "Alle Operatoren sind erlaubt." )
	  else ( False, text "Diese Operatoren sind nicht erlaubt:" 
		      <+> toDoc ds
	     )

ist_einfach = check_form 
	    $ mkSet [ "Ref", "Letter" , "Dot", "Union", "Star" ]
ist_erweitert = check_form 
	      $ mkSet [ "Ref", "Letter" , "Dot"
		      , "Union", "Intersection", "Difference", "Star" 
		      ]

einfach :: Exp -> IO String -> IO String
einfach x = moc ( ist_einfach x )

erweitert :: Exp -> IO String -> IO String
erweitert x = moc ( ist_erweitert x )
