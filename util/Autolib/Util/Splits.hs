module Util.Splits where

import List (inits, tails)

splits :: [a] -> [ ([a],[a]) ]
splits w = zip ( inits w ) ( tails w )
