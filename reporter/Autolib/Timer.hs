module Inter.Timer where

-- $Id$

import Concurrent
 
-- if timer expires,
-- write default value into channel

-- TODO: das ist eventuell zu lazy?
-- wenn die action einen nicht ganz ausgewerteten wert schreibt?

timed :: Int -> a -> IO a -> IO a
timed d def action = do
    ch <- newChan
    apid <- forkIO $ do
         x <- action
	 writeChan ch x
    tpid <- forkIO $ do
         threadDelay $ d * 10^6
	 writeChan ch def
    x <- readChan ch
    killThread apid
    killThread tpid
    return x



