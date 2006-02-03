module Autolib.Util.Seed where

-- -- $Id$

import System.Random

seed :: Int -> IO ()
seed n = do
     let nn = 314159 * n + 271828
     setStdGen $ mkStdGen nn


    
