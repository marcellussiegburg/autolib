module Autolib.Exp.Generated where

-- -- $Id$

import Autolib.Exp.Type
import Autolib.Exp.Inter

import Autolib.Shortest (is_accepted)

is_generated :: Exp -> String -> Bool
is_generated e w = 
    let a = inter_nondet std e
    in	is_accepted a w

