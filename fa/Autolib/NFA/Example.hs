module NFA.Example where

-- $Id$

import NFA

example :: NFA Char Int
example = NFA 
    { states = mkSet [ 1, 2, 3]
    , starts = mkSet [ 2]
    , finals = mkSet [ 2]
    , trans = listToFM [ ((1, 'a'), mkSet [ 2])
                       , ((2, 'a'), mkSet [ 1])
                       , ((2, 'b'), mkSet [ 3])
                       , ((3, 'b'), mkSet [ 2])
                       ]
    }
