module Reporter.Washer where

-- $Id$

import Reporter.Type
import Output

-- wash
import qualified HTMLMonad  as H
import qualified CGI        as C

washer :: Reporter a -> IO ( H.WithHTML C.CGI ( Maybe a ))
-- malt evtl. bilder (in extra files)
washer r = do
    ( mres , out :: H.WithHTML C.CGI () ) <- run r
    return $ do
        out
	return mres

