-- | Term parsing
-- implementation is a bit complicated
-- because the module should account for
-- both prefix and infix function symbols
-- and some of them may be user-defined.

module TES.In where

--  $Id$

import TES.Type
import TES.Symbol
import TES.Identifier

import Reader

import TES.Parsec
import Text.ParserCombinators.Parsec.Combinator (option)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

import Data.FiniteMap
import Data.Maybe
import Data.List ( partition )
import Control.Monad ( mzero, guard )

data Config c = Config
	      { reserved_symbols :: [ c ]
	      , allow_new_symbols :: Bool
  -- ^ if False:
  -- no user-defined function symbols of @arity > 0@,
  -- user-def. symbols of @arity == 0@ are parsed as variables
  --
  -- if True:
  -- user-defined function symbols (any @arity >= 0@) are allowed
  -- this implies that there will be no @Var v@ nodes in the parse tree
	      }
    deriving ( Show, Read )

treader :: ( Symbol c, Reader v )
	    => Config c
	    -> Parser ( Term v c )
treader conf = do
    buildExpressionParser (operator_table conf) (atomic conf)

operator_table :: ( Symbol c , Reader v )
	  => Config c
	  -> OperatorTable Char () ( Term v c )
operator_table conf  =
    do let tp = token_parser conf
       ops <- reverse $ collectBy precedence 
		      $ filter is_operator
		      $ reserved_symbols conf
       return $ do op <- ops 
		   case arity op of
		       1 -> return $ Prefix ( do 
				     reservedOp tp (show op)
				     return $ \ x -> Node op [x] )
		       2 -> return $ Infix ( do 
				     reservedOp tp (show op)
				     return $ \ x y -> Node op [x,y] ) 
				  ( assoc op )
		       _ -> [] -- ignore others (dangerous?)

collectBy :: Ord b => (a -> b) -> [a] -> [[a]]
-- from lowest to highest
collectBy f xs = 
    eltsFM $ addListToFM_C (++) emptyFM $ do
        x <- xs ; return (f x, [x])

token_parser conf = 
       let ( nulls, sonst ) = partition is_constant
			    $ reserved_symbols conf
       in makeTokenParser $ emptyDef
		   { commentLine = "" 
		   , commentStart = ""
		   , commentEnd = ""
		   , reservedNames = map show nulls
		   , reservedOpNames = map show sonst
		   }

atomic :: ( Symbol c, Reader v )
       => Config c
       -> Parser (Term v c)
atomic conf = 
      TES.Parsec.parens (token_parser conf) (treader conf)
{-
  <|> do
        t <- readerPrec 0 -- symbol (not infix!)
	mxs <- option Nothing 
			$ fmap Just
                        $ TES.Parsec.parens trs
		        $ commaSep trs
	                $ treader ops flag
        case mxs of
	     Nothing -> case flag of
			     True  -> return $ Node ( set_arity 0 t ) []
			     False -> mzero
	     Just xs -> return $ Node ( set_arity (length xs) t ) xs
-}
  <|> choice ( do op <- reserved_symbols conf
	          guard $ is_constant op
	          return $ do reserved (token_parser conf) (show op)
	                      return $ Node op []
	     )

  <|> do
         v <- reader
	 return $ Var v

instance Reader (Term Identifier Identifier ) where
    readerPrec p =  treader $ Config { reserved_symbols = [] 
				     , allow_new_symbols = True
				     }

instance Read (Term Identifier Identifier) where
    readsPrec = parsec_readsPrec
