module Util.Seed where

-- $Id$

import Random

seed :: Int -> IO ()
seed n = do
     let nn = read $ reverse $ show $ n :: Int
     setStdGen $ mkStdGen nn


    
