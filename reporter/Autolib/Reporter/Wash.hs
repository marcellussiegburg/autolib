module Autolib.Reporter.Wash where

-- -- $Id$

import Autolib.Reporter.Type

import Autolib.Output
import Autolib.Output.Wash

-- wash
import qualified Autolib.HTMLMonad  as H
import qualified Autolib.CGI        as C


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

