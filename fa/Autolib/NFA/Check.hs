module NFA.Check where

-- $Id$

import NFA
import ToDoc
import Reporter

deterministisch :: NFAC c a 
		=> NFA c a -> Reporter ()
deterministisch a = do
    inform $ text "Der Automat soll deterministisch sein."
    let non = do
	    u @ ( pc, qs ) <- fmToList $ trans a
	    guard $ cardinality qs > 1 
	    return u
    when ( not $ null non ) $ reject $ vcat
       [ text "Diese Regeln sind nicht deterministisch:"
       , nest 4 $ toDoc non
       ]
    let c = cardinality $ states a
    when ( 1 /= c ) $ reject $ vcat
       [ text "Der Automat hat nicht genau einen Startzustand,"
       , nest 4 ( text "sondern" <+> toDoc ( states a ) )
       ]
    inform $ text "Er ist es."



