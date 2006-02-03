module Autolib.Util.Chmod where

--  $Id$

import System.Cmd

-- FIXME: check for shell command injection
chmod :: String -> FilePath -> IO ()
chmod flags f = do
    system $ unwords [ "chmod",  flags, f ]
    return ()




