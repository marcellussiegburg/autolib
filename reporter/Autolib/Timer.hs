module Autolib.Timer where

--   $Id$

import Control.Concurrent
import Autolib.Reporter.Type
import Autolib.Output

-- if timer expires,
-- insert default result

timed_run :: Render r
	  => Int -- wait that many seconds
	  -> Reporter a -- default reporter
	  -> Reporter a -- time-consuming reporter
	  -> IO ( Maybe a, r )
timed_run d def r = 
    timed d ( export def) $ run r

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
    let ignore act = catch act ( \ _ -> return () ) 
    ignore $ killThread apid 
    ignore $ killThread tpid 
    return x

sleep d = sequence_ $ do
             i <- [ 1 .. d ]
	     return $ do threadDelay $ 10^6
			 yield


