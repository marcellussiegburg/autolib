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

treader :: ( Symbol c, Reader v )
	    => [c] -- ^ predefined symbols (with precedences)
	    -> Bool -- ^ True : parse unknown as symbol
		    --  False : parse unknown as variable 
	    -> Parser ( Term v c )
treader ops flag = do
    buildExpressionParser (operators ops) (atomic ops flag)


tafel ops = 
       let ( nulls, sonst ) =  partition ( \ op -> 0 == arity op ) ops
       in makeTokenParser $ emptyDef
		   { commentLine = "" 
		   , commentStart = ""
		   , commentEnd = ""
		   , reservedNames = map show nulls
		   , reservedOpNames = map show sonst
		   }

operators :: ( Symbol c , Reader v )
	  => [c]
	  -> OperatorTable Char () ( Term v c )
operators opps  =
    do ops <- reverse $ collectBy precedence 
		      $ filter ( isJust  . precedence )
		      $ opps
       return $ do op <- ops 
		   case arity op of
		       1 -> return $ Prefix ( do 
				     reservedOp (tafel opps) (show op)
				     return $ \ x -> Node op [x] )
		       2 -> return $ Infix ( do 
				     reservedOp (tafel opps) (show op)
				     return $ \ x y -> Node op [x,y] ) 
				  ( assoc op )
		       _ -> []

collectBy :: Ord b => (a -> b) -> [a] -> [[a]]
-- from lowest to highest
collectBy f xs = 
    eltsFM $ addListToFM_C (++) emptyFM $ do
        x <- xs ; return (f x, [x])

atomic :: ( Symbol c, Reader v )
       => [ c ]
       -> Bool
       -> Parser (Term v c)
atomic ops flag = 
      TES.Parsec.parens trs ( treader ops flag )
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
  <|> choice ( do op <- ops
	          guard $ arity op == 0
	          return $ do reserved (tafel ops) (show op)
	                      return $ Node op []
	     )

  <|> do
         v <- reader
	 return $ Var v

instance Reader (Term Identifier Identifier ) where
    readerPrec p =  treader [] True

instance Read (Term Identifier Identifier) where
    readsPrec = parsec_readsPrec
