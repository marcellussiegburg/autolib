module Util.Factor where

-- -- $Id$

import List ( tails, isPrefixOf )

factor :: Eq a => [a] -> [a] -> Bool
factor x y = or $ do
    z <- tails y
    return $ isPrefixOf x z
