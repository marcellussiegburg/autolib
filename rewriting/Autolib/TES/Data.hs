
-- | implements term rewriting systems
-- as represented in the .tes-format

module TES.Data where

import TES.Symbol
import TES.Term
import TES.Position (syms)
import TES.Rule
import TES.Identifier

import Sets

import ToDoc
import Reader

import TES.Parsec


data TRS v c  = TRS
	 { comment :: String
	 , variables :: Set v -- ^ nullary symbols
	 , signature :: Set c -- ^ all symbols (not including variables, I hope)
	 , rules :: [ Rule v c ]
	 }

type TES = TRS Identifier Identifier

instance ( TRSC v c ) 
	 => ToDoc ( TRS v c ) where
    toDoc t = vcat $ text ( "# " ++ comment t )
		   : toDoc ( setToList $ variables t )
		   : map toDoc ( rules t )

instance ( TRSC v c )
	 => Show ( TRS v c ) where show = render . toDoc

instance Reader TES where
    readerPrec p = do
        whiteSpace tes_prefix

        vs <- option -- wild guess: if keine vars angegeben:
	             [ mknullary "x", mknullary "y", mknullary "z" ]
	      $ readerPrec 0 :: Parser [ Identifier ]
	let vars =  mkSet $ do v <- vs ; return $ v { i_arity = 0 }

        whiteSpace tes_prefix -- kommentare erlaubt
	rs <- many ( readerPrec 0 ) -- regeln

        whiteSpace tes_prefix -- kommentare erlaubt
	option () $ foot

	let sig = sfilter ( \ s -> not (s `elementOf` vars)) 
		$ symbols rs

	-- change (some) nullary ids to vars
	let xform ( Node c [] ) | c `elementOf` vars = Var c
	    xform ( Node c args ) = Node c ( map xform args )

        return $ TRS { comment = ""
		     , variables = vars 
		     , rules = do (l, r) <- rs
				  return ( xform l, xform r )
		     , signature = sig 
		     }

foot :: Parser ()
foot = do
    reservedOp tes "~" <|> reserved tes "COMMENT"
    many $ satisfy (const True) -- ignore
    return ()




instance Read TES where
    readsPrec = parsec_readsPrec


symbols :: Ord c => [ Rule v c ] -> Set c
symbols rules = unionManySets $ do
    (l, r) <- rules
    [ syms l , syms r ]

at_most_unary :: TRSC v c => TRS v c -> Bool
at_most_unary tes = and $ do
    s <- setToList $ signature tes
    return $ arity s <= 1

has_nullary :: TRSC v c => TRS v c -> Bool
has_nullary tes = or $ do
    s <- setToList $ signature tes
    return $ arity s == 0
