module Autolib.Reporter.Subset where

--  $Id$

import Autolib.Reporter.Type
import Autolib.Sets
import Autolib.ToDoc
import Control.Monad ( when )

check :: ( Ord a , ToDoc [a] )
     => ( Doc, Set a )
     -> ( Doc, Set a )
     -> Reporter ()
check ( d1, s1 ) ( d2, s2 ) = do
    inform $ vcat
	   [ text "Ist die Menge"
	   , nest 4 $ fsep [ d1, equals, toDoc s1 ]
	   , text "Teilmenge der Menge"
	   , nest 4 $ fsep [ d2, equals, toDoc s2 ]
	   , text "?"
	   ]
    let no = minusSet s1 s2
    when ( not $ isEmptySet no ) $ reject $ vcat
	     [ text "Nein, diese Elemente sind"
	     , text "in" <+> d1 
	     , text ", aber nicht in" <+> d2
	     , toDoc no
	     ]
    inform $ text "Ja."
