module Autolib.NFA.Example where

-- -- $Id$

import Autolib.NFA.Type
import Autolib.ToDoc
import Autolib.Set

example :: NFA Char Int
example = example_sigma $ mkSet "ab"

example_sigma :: Set Char -> NFA Char Int
example_sigma s =
    let a : b : _ = setToList s
    in  NFA 
    { nfa_info = funni "example" [ info s ]
    , alphabet = s
    , states = mkSet [ 1, 2, 3]
    , starts = mkSet [ 2]
    , finals = mkSet [ 2]
    , trans = listToFM [ ((1, a), mkSet [ 2])
                       , ((2, a), mkSet [ 1, 3])
                       , ((2, b), mkSet [ 3])
                       , ((3, b), mkSet [ 2])
                       ]
    }
