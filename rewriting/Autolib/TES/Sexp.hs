-- | 'S Expressions' as used in WST04 syntax
--  (for comments and such)

module TES.Sexp where

import qualified TES.Parsec
import ToDoc
import Reader

data Sexp = Leaf String
	  | List [ Sexp ]

wrap :: Show a => String ->  [a] -> Sexp
wrap f args = List
	    $ Leaf f 
	    : map ( Leaf . show ) args 

instance ToDoc Sexp where
    toDoc ( Leaf x ) = text x
    toDoc ( List [] ) = parens empty
    toDoc ( List (x : xs) ) = 
	    parens $  toDoc x <+> vcat ( map toDoc xs )

instance Show Sexp where show = render . toDoc

instance Reader Sexp where
    reader = do f <- TES.Parsec.pseudo_identifier ; return $ Leaf f
         <|> my_parens ( 
                      do ts <- many reader 
			 return $ List ts
		    )

instance Read Sexp where readsPrec = parsec_readsPrec

