module Autolib.Exp.Example where

-- -- $Id$

import Autolib.Exp.Type
import Autolib.Set

example :: Set Char -> Exp
example alpha = 
    let a : b : _ = setToList alpha
    in PowerStar ( Union (Letter a) (Dot (Letter b) (Letter a)))

    