module Autolib.Util.Splits where

import Data.List (inits, tails)

splits :: [a] -> [ ([a],[a]) ]
splits w = zip ( inits w ) ( tails w )
