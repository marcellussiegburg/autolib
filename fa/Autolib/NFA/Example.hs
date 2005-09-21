module Autolib.NFA.Example where

-- -- $Id$

import Autolib.NFA.Type
import Autolib.ToDoc
import Autolib.Set

example :: NFA Char Int
example = example_sigma $ mkSet "ab"

example_sigma :: Set Char -> NFA Char Int
example_sigma s = NFA 
    { nfa_info = funni "example" [ info s ]
    , alphabet = s
    , states = mkSet [ 1, 2, 3]
    , starts = mkSet [ 2]
    , finals = mkSet [ 2]
    , trans = collect $
         case setToList s of
              a : b : _ -> 
                  [ (1, a, 2), (2, a, 1), (2, a, 3) 
                  , (2, b, 3), (3, b, 2)
                  ]
              [ a ] -> [  (1, a, 2), (2, a, 1), (2, a, 3) ]
              [] -> []
    }
