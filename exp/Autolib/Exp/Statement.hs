module Exp.Statement 

-- $Id$

where

import Exp.Type
import Exp.Read
import Exp.Print

import Parsec
import ParsecExpr
import Exp.MyTokens

data Statement 
     = Print  Exp
     | Display Exp
     | Let String Exp

instance Show Statement where
    -- TODO: ist das richtig geklammert?
    show (Print x)  = "print " ++ show x ++ ";"
    show (Display x)  = "display " ++ show x ++ ";"
    show (Let v x) = "let " ++ v ++ " = " ++ show x ++ ";"


--------------------------------------------------------------------------

program :: Parser [ Statement ]
program = do
    stats <- many statement
    eof
    return stats

statement :: Parser Statement
statement = do
    s <- binding <|> printer <|> displayer
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


displayer = do
    reserved "display"
    x <- expression
    return $ Display x
  <?> "display statement"




