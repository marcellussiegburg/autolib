module Exp.Statement 

-- -- $Id$

where

import Exp.Type
import Exp.Read
import Exp.Print

import Text.ParserCombinators.Parsec

import Exp.MyTokens

data Statement 
     = Print  Exp
     | Display Exp
     | Let String Exp
     | Quit

instance Show Statement where
    -- TODO: ist das richtig geklammert?
    show (Quit) = "quit"
    show (Print x)  = "print " ++ show x 
    show (Display x)  = "display " ++ show x 
    show (Let v x) = "let " ++ v ++ " = " ++ show x 


--------------------------------------------------------------------------

program :: Parser [ Statement ]
program = do
    stats <- many ( do s <- statement ; reservedOp ";" ; return s )
    eof
    return stats

statement :: Parser Statement
statement = 
      binding <|> printer <|> displayer <|> quitter
--  <?> "statement"

binding = do
    reserved "let"
    lhs <- name
    reservedOp "="
    rhs <- expression
    return $ Let lhs rhs
--  <?> "let binding"    

name = do
    c <- upper
    cs <- many alphaNum
    whiteSpace
    return $ c : cs
--  <?> "name (starting with upper case letter)"

printer = do
    reserved "print"
    x <- expression
    return $ Print x
--  <?> "print statement"


displayer = do
    reserved "display"
    x <- expression
    return $ Display x
--  <?> "display statement"

quitter = do
    reserved "quit"
    return $ Quit





