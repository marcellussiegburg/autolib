module Reporter.Wash where

-- -- $Id$

import Reporter.Type

import Output
import Output.Wash

-- wash
import qualified HTMLMonad  as H
import qualified CGI        as C


-- for use  with wash/cgi
embed :: Monad m => Reporter a -> H.WithHTML m ( Maybe a )
embed r = do
    let ( res, com :: m () ) = export r
    com
    return res



washer :: Reporter a -> IO ( H.WithHTML C.CGI ( Maybe a ))
-- malt evtl. bilder (in extra files)
washer r = do
    ( mres , out :: H.WithHTML C.CGI () ) <- run r
    return $ do
        out
	return mres

