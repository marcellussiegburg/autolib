module Autolib.TES.Out where

--  $Id$

import Autolib.TES.Type
import Autolib.Symbol

import Autolib.ToDoc
import Autolib.ToTex

instance ( Symbol c, ToDoc v ) => ToDoc (Term v c) where
    toDocPrec _ ( Var v ) = toDoc v
    toDocPrec p ( Node t xs ) = 
	case ( arity t, precedence t ) of
             ( 2, Just q ) -> ( if p >= q then Autolib.ToDoc.parens else id )
			    $ fsep [ toDocPrec q ( xs !! 0)
				   , toDoc t <+> toDocPrec q ( xs !! 1)
				   ]
	     ( 1, Just q ) -> ( if p > q then Autolib.ToDoc.parens else id )
			    $ toDoc t <+> toDocPrec q ( xs !! 0)
	     _ -> toDoc t <+> 
		if null xs 
		then Autolib.ToDoc.empty 
		else Autolib.ToDoc.parens $ hcat $ punctuate ( text ", " )
			    $ map toDoc xs

instance ( ToDoc v, Symbol c, ToTex v, ToTex c ) 
         => ToTex ( Term v c ) where
    toTex ( Var v ) = toTex v
    toTex ( Node c [] ) = toTex c
    toTex ( Node c [l, r] ) = 
        Macro "tree" [ Opt $ toTex c, Req $ toTex l, Req $ toTex r ]

