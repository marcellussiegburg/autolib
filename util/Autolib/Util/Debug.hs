module Util.Debug

-- $Id$

where

debugging :: Bool
debugging = False

debug :: String -> IO ()
debug msg = when debugging $ putStrLn msg