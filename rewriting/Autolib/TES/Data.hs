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
        whiteSpace tes
        vs <- option [] $ readerPrec 0 :: Parser [ Symbol ]
	let vars =  mkSet $ do v <- vs ; return $ v { arity = 0 }

        whiteSpace tes
	rs <- many ( readerPrec 0 ) -- regeln
	let sig = sfilter ( \ s -> not (s `elementOf` vars)) 
		$ symbols rs
        return $ TES { variables = vars , rules = rs , signature = sig }

instance Read TES where
    readsPrec = parsec_readsPrec


symbols :: [ Rule ] -> Set Symbol
symbols rules = mkSet $ do
    (l, r) <- rules
    flatten l ++ flatten r
