module Autolib.Reporter.Set where

--  $Id$

import Autolib.Reporter.Type
import Autolib.Set
import Autolib.ToDoc
import Control.Monad ( when )

subeq :: ( Ord a , ToDoc [a] )
     => ( Doc, Set a )
     -> ( Doc, Set a )
     -> Reporter ()
subeq ( d1, s1 ) ( d2, s2 ) = do
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

eq ::  ( Ord a , ToDoc [a] )
     => ( Doc, Set a )
     -> ( Doc, Set a )
     -> Reporter ()
eq m1 @ ( d1, s1 ) m2 @ ( d2, s2 ) = do
    inform $ vcat
	   [ text "Stimmen die Menge"
	   , nest 4 $ fsep [ d1, equals, toDoc s1 ]
	   , text "und die Menge"
	   , nest 4 $ fsep [ d2, equals, toDoc s2 ]
	   , text "überein?"
	   ]
    nested 4 $ do
        subeq m1 m2
        subeq m2 m1
