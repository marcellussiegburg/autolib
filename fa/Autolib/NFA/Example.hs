module NFA.Example where

-- $Id$

import NFA
import ToDoc
import Sets

example :: NFA Char Int
example = example_sigma $ mkSet "ab"

example_sigma :: Set Char -> NFA Char Int
example_sigma s =
    let a : b : _ = setToList s
    in  NFA 
    { nfa_info = text "example"
    , states = mkSet [ 1, 2, 3]
    , starts = mkSet [ 2]
    , finals = mkSet [ 2]
    , trans = listToFM [ ((1, a), mkSet [ 2])
                       , ((2, a), mkSet [ 1, 3])
                       , ((2, b), mkSet [ 3])
                       , ((3, b), mkSet [ 2])
                       ]
    }
