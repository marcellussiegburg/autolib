module Autolib.Logic.Formula.FO.Reader where

import Autolib.Logic.Formula.FO.Data
import Autolib.Logic.Formula.Name

import Autolib.Reader
import Text.Parsec.Expr

expr = do
    f <- exprF undefined
    return $ f undefined

exprF :: ( String -> n )
      -> Parser ( Form n )
exprF env = buildExpressionParser
    [ [ prefix "not" Not  ]
    , [ binary "&&" And AssocLeft ]
    , [ binary "||" Or AssocLeft ]
    , [ binary "=>" Implies AssocRight ]
    ] ( atomF env )

atomF env = my_parens (exprF  env)
    <|> quantifiedF env
    <|> basicF env


quantifiedF env = 
       ( my_reserved "forall" >> contF Forall env )
   <|> ( my_reserved "exists" >> contF Exists env )

contF q env = do
    n <- my_identifier ; my_symbol ":"
    let env' = x( my_reserved n >> return Env ) <|> env
    body <- exprF env'
    return $ \ val0 -> q $ \ val1 -> body val1 -- WHAT

basicF :: Parser (n -> Form n)
       -> Parser (n -> Form n)
basicF env = do
    x <- env ; my_symbol "<"; y <- env
    return $ \ val -> 
        Less (unEnv $ x val) (unEnv $ y val)

unEnv (Env v) = v

{-
basicF var = name >>= \ x -> case resolve env x of
        Nothing -> do
            y <- reference env
            return $ Letter x y
        Just x -> 
             do my_reservedOp "<1" ; y <- reference env
                return $ Succ x y
         <|> do my_reservedOp "<" ; y <- reference env
                return $ Less x y
-}

prefix  name fun       = 
    Prefix (do{ my_reserved name; return fun })
binary  name fun assoc = 
    Infix (do{ my_symbol name; return fun }) assoc
