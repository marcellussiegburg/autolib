
-- $Header$

module Exp.Read where

import Exp.Type

import Char

import Parsec
import ParsecExpr

import Exp.MyTokens

--------------------------------------------------------------------------

express :: Parser (Exp, String)
express = do 
    whiteSpace
    x <- expression
    rest <- getInput
    return (x, rest)

expression =  buildExpressionParser operators catenation

operators =
    [ [ op "*" Dot          AssocLeft 
      , op "\\" Left_Quotient     AssocLeft 
      , op "/" Right_Quotient     AssocLeft 
      ]
    , [ op "$" Shuffle      AssocLeft ]
    , [ op "&" Intersection AssocLeft ]
    , [ op "+" Union AssocLeft
      , op "-" Difference AssocLeft  
      , op "<>" SymDiff AssocLeft  
      ]
    ]
    where
      op name f assoc   = 
	 Infix ( do { symbol name; return f } 
		 <?> "operator" ) assoc

catenation :: Parser Exp
catenation = do
    ps <- many1 monomial
    return $ foldr1 Dot ps

monomial :: Parser Exp
monomial = do
    x <- atom
    fs <- many $
	    do symbol "^" 
	       (     do symbol  "+" ; return $ Plus  
		 <|> do symbol  "*" ; return $ Star   
		 <|> do e <- natural; return $ Power e
		     -- TODO: "hoch mod n"
		 <?> "exponent"
		 ) 
    return $ foldl (.) id (reverse fs) $ x

atom :: Parser Exp
atom =      parens expression 
       <|>  do b <- basic ; whiteSpace; return b
       <?>  "atomic expression"

basic :: Parser Exp
basic = do
    c <-  alphaNum
    if isUpper c
       then do cs <- many alphaNum
	       return $ Ref (c : cs)
       else    return $ Letter c

--------------------------------------------------------------------------

instance Read Exp where
    readsPrec _ inp =
        case parse express "<stdin>" inp of
	     Right (x, rest) -> return (x, rest)
	     Left err -> error ("\n" ++ inp ++ "\n" ++ show err)


--------------------------------------------------------------------------

program :: Parser [ Statement ]
program = do
    stats <- many statement
    eof
    return stats

statement :: Parser Statement
statement = do
    s <- binding <|> printer
    reservedOp ";"
    return s
  <?> "statement"

binding = do
    reserved "let"
    lhs <- name
    reservedOp "="
    rhs <- expression
    return $ Let lhs rhs
  <?> "let binding"    

name = do
    c <- upper
    cs <- many alphaNum
    whiteSpace
    return $ c : cs

printer = do
    reserved "print"
    x <- expression
    return $ Print x
  <?> "print statement"



