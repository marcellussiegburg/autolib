-- see discussion at
-- http://stackoverflow.com/questions/19415621/parsing-for-phoas-expressions

module Autolib.Logic.Formula.FO.Reader where

import Autolib.Logic.Formula.FO.Data
import Autolib.Logic.Formula.FO.ToDoc () -- instance
import Autolib.Logic.Formula.Name

import Autolib.Reader
import Text.Parsec.Expr
import Text.Parsec.Prim ( sysUnExpectError )

import qualified Data.Set as S
import qualified Data.Map as M

instance Reader Formula where reader = formula

formula :: Parser Formula
formula = do
    f <- expr S.empty
    return $ Formula $ convert undefined f 

-- this is what I was hoping to avoid:
-- going via plain abstract syntax
convert :: (Name -> n) -> E -> Form n
convert env e = case e of
    ELess x y -> Less (env x) (env y)
    ESucc x y -> Succ (env x) (env y)
    ELetter x y -> Letter x (env y)
    ENot f -> Not ( convert env f )
    EAnd f g -> And (convert env f)(convert env g)
    EOr f g -> Or (convert env f)(convert env g)
    EImplies f g -> Implies (convert env f)(convert env g)
    EForall n f -> Forall $ \ a -> 
        convert (extend env n a) f
    EExists n f -> Exists $ \ a -> 
        convert (extend env n a) f

extend env name val = \ v -> 
    if v == name then val else env v

data E = ELess Name Name
       | ESucc Name Name
       | ELetter Char Name
       | ENot E | EAnd E E | EOr E E | EImplies E E
       | EForall Name E | EExists Name E
    deriving ( Show )

expr :: S.Set Name -> Parser E
expr bound = buildExpressionParser
    [ [ prefix "not" ENot  ]
    , [ binary "&&" EAnd AssocLeft ]
    , [ binary "||" EOr AssocLeft ]
    , [ binary "=>" EImplies AssocRight ]
    ] ( atom  bound )

atom bound = my_parens (expr bound)
    <|> quantified bound
    <|> basic bound

quantified bound = 
       ( my_reserved "forall" >> cont EForall bound )
   <|> ( my_reserved "exists" >> cont EExists bound )

cont q bound = do
    var <- name ; my_symbol ":"
    body <- expr $ S.insert var bound
    return $ q var body

basic bound = name >>= \ x -> 
    if S.member x bound
    then do
        s <-  try ( my_symbol "<1" >> return ESucc )
          <|> ( my_symbol "<"  >> return ELess )
        y <- reference bound
        return $ s x y
    else case show x of
        [c] -> do
            y <- my_parens (reference bound)
            return $ ELetter c y
        s -> fail $ "not a letter: " ++ s

name = fmap Name my_identifier

-- TODO: how to set the error position?
reference :: S.Set Name -> Parser Name
reference bound = do
    n <- name
    if S.member n bound
        then return n
        else fail $ "name not bound: " ++ show n

prefix  name fun       = 
    Prefix (do{ my_reserved name; return fun })
binary  name fun assoc = 
    Infix (do{ my_symbol name; return fun }) assoc
