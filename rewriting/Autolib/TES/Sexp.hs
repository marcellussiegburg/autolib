-- | 'S Expressions' as used in WST04 syntax
--  (for comments and such)

module TES.Sexp where

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
	    parens $  toDoc x <+> fsep ( map toDoc xs )

instance Show Sexp where show = render . toDoc

instance Reader Sexp where
    reader = do f <- my_identifier ; return $ Leaf f
         <|> my_parens ( 
                      do ts <- many reader 
			 return $ List ts
		    )

instance Read Sexp where readsPrec = parsec_readsPrec

