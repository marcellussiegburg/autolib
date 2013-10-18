module Autolib.Logic where

import Autolib.Logic.Formula.FO as F
import Autolib.Logic.Formula.SO as S

import Autolib.Logic.Transform
import Autolib.Logic.NFA

import Autolib.NFA
import qualified Data.Set as S

semantics :: S.Set Char -> F.Formula -> NFA Char Int
semantics alpha f = 
    let s = fo2mso0 f
        a = to_nfa (S.toList alpha) 0 s
    in  alphamap ( \(c,[]) -> c ) a

