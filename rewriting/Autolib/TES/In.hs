module TES.In where

--  $Id$

import TES.Type
import TES.Symbol
import TES.Identifier

import Reader

import TES.Parsec
import Text.ParserCombinators.Parsec.Combinator (option)
import Text.ParserCombinators.Parsec.Expr

import Data.FiniteMap
import Data.Maybe

treader :: ( Symbol c, Read v )
	    => [c] -- ^ predefined symbols (with precedences)
	    -> Bool -- ^ True : parse unknown as symbol
		    --  False : parse unknown as variable 
	    -> Parser ( Term v c )
treader ops flag = do
    buildExpressionParser (operators ops) (atomic ops flag)


operators :: ( Symbol c , Read v )
	  => [c]
	  -> OperatorTable Char () ( Term v c )
operators ops  =
    do ops <- reverse $ collectBy precedence 
		      $ filter ( isJust  . precedence )
		      $ ops
       return $ do op <- ops 
		   case arity op of
		       1 -> return $ Prefix ( do 
				     symbol trs (show op)
				     return $ \ x -> Node op [x] )
		       2 -> return $ Infix ( do 
				     symbol trs (show op)
				     return $ \ x y -> Node op [x,y] ) 
				  AssocLeft
		       _ -> []

collectBy :: Ord b => (a -> b) -> [a] -> [[a]]
-- from lowest to highest
collectBy f xs = 
    eltsFM $ addListToFM_C (++) emptyFM $ do
        x <- xs ; return (f x, [x])

atomic :: ( Symbol c, Read v )
       => [ c ]
       -> Bool
       -> Parser (Term v c)
atomic ops flag = 
      TES.Parsec.parens trs ( treader ops flag )
  <|> do
        t <- readerPrec 0 -- symbol
	mxs <- option Nothing 
			$ fmap Just
                        $ TES.Parsec.parens trs
		        $ commaSep trs
	                $ treader ops flag
        return $ case mxs of
	     Nothing -> case flag of
			     True  -> Node ( set_arity 0 t ) []
			     False -> Var  ( read $ show t )
	     Just xs -> Node ( set_arity (length xs) t ) xs

instance Reader (Term Identifier Identifier ) where
    readerPrec p =  treader [] True

instance Read (Term Identifier Identifier) where
    readsPrec = parsec_readsPrec
