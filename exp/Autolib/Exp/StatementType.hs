module StatementType where

import ExpType

data Statement 
     = Print  Exp
     | Let String Exp
