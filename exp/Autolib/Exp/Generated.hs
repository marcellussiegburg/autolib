module Exp.Generated where

-- $Id$

import Exp.Type
import Exp.Inter

import Shortest (is_accepted)

is_generated :: Exp -> String -> Bool
is_generated e w = 
    let a = inter_nondet std e
    in	is_accepted a w

