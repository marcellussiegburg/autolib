module Autolib.Util.Chmod where

--  $Id$

import System

chmod :: String -> FilePath -> IO ()
chmod flags f = do
    system $ unwords [ "chmod",  flags, f ]
    return ()




