-- -- $Id$

module NFA.Restrict where

import NFA.Type

import Sets
import Data.FiniteMap
import Size

import ToDoc
import Control.Monad (guard, when)
import Reporter.Type

restrict_states
   :: NFAC c a
   => NFA c a -> Reporter ()
restrict_states a = do
    let forbidden q = not ( q `elementOf` states a )

    let nostarts  = sfilter forbidden $ starts a
    when ( nonempty nostarts ) $ reject $ vcat
	 [ text "Diese Startzustände sind nicht deklariert:"
	 , nest 4 $ toDoc nostarts
	 ]

    let nofinals  = sfilter forbidden $ finals a
    when ( nonempty nofinals ) $ reject $ vcat
	 [ text "Diese akzeptierenden Zustände sind nicht deklariert:"
	 , nest 4 $ toDoc nofinals 
	 ]

    let norules   = do
	    rule @ ((p, x), qs) <- ltrans a
	    q <- setToList qs
	    guard $ forbidden p || forbidden q
	    return rule
    when ( not $ null norules ) $ reject $ vcat
	 [ text "Diese Übergangsregeln benutzen nicht deklarierte Zustände:"
	 , nest 4 $ toDoc norules
	 ]
    inform $ text "OK: Alle benutzten Zustände wurden auch deklariert."


restrict_alpha
   :: NFAC c a
   => Set c -> NFA c a 
   -> Reporter ()
restrict_alpha alpha a = do
    let	noletters = do
	    rule @ ((p, x), qs) <- ltrans a
	    q <- setToList qs
	    guard $ not (x `elementOf` alpha)
	    return rule
    when ( not $ null noletters ) $ reject $ vcat
	 [ text "Diese Übergangsregeln benutzen nicht erlaubte Buchstaben:"
	 , nest 4 $ toDoc noletters
	 ]
    inform $ text "OK: Die Übergangsregeln benutzen nur erlaubte Buchstaben."

restrict_size
   :: NFAC c a
   => Int -> NFA c a 
   -> Reporter ()
restrict_size s a = do
    inform $ fsep [ text "Der Automat soll höchstens", toDoc s
		  , text "Zustände haben."
		  , text "Er besitzt", toDoc (size a), text "."
		  ]
    when ( size a > s ) $ reject $ text "Das ist zuviel."
    inform $ text "Das ist OK."

