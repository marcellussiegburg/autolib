module Autolib.Util.Debug

-- -- $Id$

where

import Control.Monad (when)

debugging :: Bool
debugging = False

debug :: String -> IO ()
debug msg = when debugging $ putStrLn msg