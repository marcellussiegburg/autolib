{-# OPTIONS -fglasgow-exts  #-}

-- | Term parsing
-- implementation is a bit complicated
-- because the module should account for
-- both prefix and infix function symbols
-- and some of them may be user-defined.

module Autolib.TES.In where

--  $Id$

import Autolib.TES.Type
import Autolib.Symbol
import Autolib.TES.Identifier

import Autolib.Reader

import Autolib.TES.Parsec
import Text.ParserCombinators.Parsec.Combinator (option)
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language

import Autolib.FiniteMap
import Data.Maybe
import Data.List ( partition )
import Control.Monad ( mzero, guard )

data Config c = Config
        -- FIXME:  nullary symbols are in _operator_ now,
        -- should go into _function_ 
	      { reserved_operator_symbols :: [ c ]
              , reserved_function_symbols :: [ c ]
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
		      $ reserved_operator_symbols conf
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

-- | from lowest to highest
collectBy :: Ord b => (a -> b) -> [a] -> [[a]]
collectBy f xs = 
    eltsFM $ addListToFM_C (++) emptyFM $ do
        x <- xs ; return (f x, [x])

token_parser conf = 
       let ( nulls, sonst ) = partition is_constant
			    $ reserved_operator_symbols conf
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
  let tp = token_parser conf 
  in
          Autolib.TES.Parsec.parens tp (treader conf)
      <|> choice ( do op <- reserved_operator_symbols conf
      	              guard $ is_constant op
      	              return $ do reserved tp (show op)
      	                          return $ Node op []
      	         )
      <|> choice ( do t <- reserved_function_symbols conf
                      return $ do
                          reserved tp ( show t )
                          xs <- Autolib.TES.Parsec.parens tp
                               $ commaSep tp
                               $ treader conf
                          return $ Node ( set_arity (length xs) t ) xs
                 )
      <|> if allow_new_symbols conf
          then do 
              t <- readerPrec 0 
      	      xs <- option []
                        $ Autolib.TES.Parsec.parens tp
      		        $ commaSep tp
      	                $ treader conf
      	      return $ Node ( set_arity (length xs) t ) xs
          else do
             v <- reader
       	     return $ Var v

-- instance Reader (Term Identifier Identifier ) where
instance ( Symbol c, Reader v ) => Reader ( Term v c ) where
    readerPrec p =  treader $ Config { reserved_operator_symbols = [] 
                                     , reserved_function_symbols = [] 
				     , allow_new_symbols = True
				     }


