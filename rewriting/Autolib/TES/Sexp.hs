-- | 'S Expressions' as used in WST04 syntax
--  (for comments and such)

module Autolib.TES.Sexp where

import qualified Autolib.TES.Parsec
import Autolib.ToDoc
import Data.List (intersperse)
import Autolib.Reader

data Sexp = Leaf String
	  | List [ Sexp ]
    deriving ( Eq, Ord )

wrap :: ToDoc a => String ->  [a] -> Sexp
wrap f args = List
	    $ Leaf ( f) 
	    : map ( Leaf . render . toDoc ) args 

wrap_sep :: ToDoc a => String -> String ->  [a] -> Sexp
wrap_sep sep f args = List
	    $ Leaf (  f ) 
	    : ( intersperse ( Leaf $  sep ) $ map ( Leaf . render . toDoc ) args  )

instance ToDoc Sexp where
    toDoc ( Leaf x ) = text x
    toDoc ( List [] ) = parens empty
    toDoc ( List (x : xs) ) = 
	    parens $  toDoc x <+> zippo xs

zippo [] = empty
zippo [x] = toDoc x
zippo ( x : y : rest ) = vcat
      [ toDoc x <+> toDoc y 
      , zippo rest
      ]


instance Reader Sexp where
    reader = do f <- Autolib.TES.Parsec.pseudo_identifier ; return $ Leaf $  f
         <|> Autolib.TES.Parsec.parens Autolib.TES.Parsec.trs ( 
                      do ts <- many reader 
			 return $ List ts
		    )



