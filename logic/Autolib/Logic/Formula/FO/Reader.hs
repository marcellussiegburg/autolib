module Autolib.Logic.Formula.FO.Reader where

import Autolib.Logic.Formula.FO.Data
import Autolib.Logic.Formula.Name

import Autolib.Reader
import Text.Parsec.Expr

-- WHY NOT? expr :: Parser (forall n . Form n) 
expr = do
    f <- exprF ( fail "global variable" )
    return $ f undefined

exprF :: Parser (n -> Form n) 
      -> Parser (n -> Form n)
exprF var = buildExpressionParser
    [ [ prefix "not" notF  ]
    , [ binary "&&" andF AssocLeft ]
    , [ binary "||" orF AssocLeft ]
    , [ binary "=>" impliesF AssocRight ]
    ] ( atomF var )

notF f = \ val -> Not (f val)
andF f g = \ val -> And (f val) (g val)
orF f g = \ val -> Or (f val) (g val)
impliesF f g = \ val -> Implies (f val) (g val)

atomF var = my_parens (exprF  var)
    <|> quantifiedF var
    <|> basicF var


quantifiedF var = 
       ( my_reserved "forall" >> contF Forall var )
   <|> ( my_reserved "exists" >> contF Exists var )


contF :: ( (n -> Form n) -> Form n )
      -> Parser (n -> Form n)
      -> Parser (n -> Form n)
contF q var = do
    n <- my_identifier ; my_symbol ":"
    let var' = ( my_reserved n >> return Var ) <|> var
    body <- exprF var'
    return $ \ val0 -> q $ \ val1 -> body val1 -- WHAT

basicF :: Parser (n -> Form n)
       -> Parser (n -> Form n)
basicF var = do
    x <- var ; my_symbol "<"; y <- var
    return $ \ val -> 
        Less (unVar $ x val) (unVar $ y val)

unVar (Var v) = v

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
