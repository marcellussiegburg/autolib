module Util.Debug

-- -- $Id$

where

import Monad (when)

debugging :: Bool
debugging = False

debug :: String -> IO ()
debug msg = when debugging $ putStrLn msg