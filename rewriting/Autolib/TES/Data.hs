module TES.Data where

-- implements term rewriting systems
-- as represented in the .tes-format

import TES.Symbol
import TES.Term
import TES.Rule

import Sets
import Tree

import ToDoc
import Reader

import Parsec
import ParsecToken
import TES.Parsec


data TES = TES
	 { comment :: String
	 , variables :: Set Symbol -- nullary symbols
	 , signature :: Signature
	 , rules :: [ Rule ]
	 }

instance ToDoc TES where
    toDoc t = vcat $ text ( "# " ++ comment t )
		   : toDoc ( setToList $ variables t )
		   : map toDoc ( rules t )

instance Show TES where show = render . toDoc

instance Reader TES where
    readerPrec p = do
        whiteSpace tes_prefix

        vs <- option -- wild guess: if keine vars angegeben:
	             [ mknullary "x", mknullary "y", mknullary "z" ]
	      $ readerPrec 0 :: Parser [ Symbol ]
	let vars =  mkSet $ do v <- vs ; return $ v { arity = 0 }

        whiteSpace tes_prefix -- kommentare erlaubt
	rs <- many ( readerPrec 0 ) -- regeln

        whiteSpace tes_prefix -- kommentare erlaubt
	option () $ foot

	let sig = sfilter ( \ s -> not (s `elementOf` vars)) 
		$ symbols rs
        return $ TES { comment = ""
		     , variables = vars , rules = rs , signature = sig 
		     }

foot = do
    reservedOp tes "~" <|> reserved tes "COMMENT"
    many $ satisfy (const True) -- ignore
    return ()

instance Read TES where
    readsPrec = parsec_readsPrec


symbols :: [ Rule ] -> Set Symbol
symbols rules = mkSet $ do
    (l, r) <- rules
    flatten l ++ flatten r

at_most_unary :: TES -> Bool
at_most_unary tes = and $ do
    s <- setToList $ signature tes
    return $ arity s <= 1

has_nullary :: TES -> Bool
has_nullary tes = or $ do
    s <- setToList $ signature tes
    return $ arity s == 0
