module Inter.Timer where

-- $Id$

import Concurrent
 
-- if timer expires,
-- insert default result

-- TODO: das ist eventuell zu lazy?
-- wenn die action einen nicht ganz ausgewerteten wert schreibt?

timed :: Int -> a -> IO a -> IO a
timed d def action = do
    ch <- newChan

    apid <- forkIO $ do
         x <- action
	 writeChan ch x

    tpid <- forkIO $ do
         sleep d
	 writeChan ch def

    x <- readChan ch
    -- killThread apid    
    -- killThread tpid
    return x

sleep d = sequence_ $ do
             i <- [ 1 .. d ]
	     return $ do threadDelay $ 10^6
			 yield


