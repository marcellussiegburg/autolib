module Statement 

( module Statement
, module StatementType
)

where

import Exp
import StatementType

instance Show Statement where
    show (Print x)  = "print " ++ show x ++ ";"
    show (Let v x) = "let " ++ v ++ " = " ++ show x ++ ";"



