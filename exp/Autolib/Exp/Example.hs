module Exp.Example where

-- -- $Id$

import Exp.Type
import Sets

example :: Set Char -> Exp
example alpha = 
    let a : b : _ = setToList alpha
    in Star ( Union (Letter a) (Dot (Letter b) (Letter a)))

    