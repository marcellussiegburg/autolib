module Inter.Timer where

-- $Id$

import qualified Concurrent

timer :: Int -> IO ()
-- throws exception after specified number of seconds

-- by angelic parallelism (?),
-- if the main thread finishes earlier than the timer,
-- the timer is silently killed (I hope)
timer d = do
    Concurrent.threadDelay $ d * 10^6
    error "timer expired"

