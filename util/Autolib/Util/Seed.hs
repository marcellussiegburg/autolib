module Util.Seed where

import Random

seed :: Int -> IO ()
seed n = do
     setStdGen $ mkStdGen n

    
