module Autolib.NFA.Interface where

--  $Id$

import Autolib.Interpreter.Function
import Autolib.NFA.Type
import Autolib.NFA.Ops
import Autolib.NFA.Normalize
import Autolib.NFA.Det
import Autolib.NFA.Minimize
import Autolib.NFA.Minus

functions :: [ Function ( NFA Char Int ) ]
functions = 
    [ binary "union" normalize_union 
    , binary "intersection" normalize_intersection 
    , unary "complement" $ \ a -> complement ( setToList $ alphabet a ) a
    , binary "minus" minus 

    , binary "dot" dot 
    , unary "plus" plus
    , unary "star" star

    , unary "deterministic" ( normalize . det )
    , unary "minimize" minimize 
    ]

    