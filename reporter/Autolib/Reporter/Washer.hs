module Reporter.Washer where

-- $Id$

import Reporter.Type
import Output
import HTMLMonad -- wash

washer :: FilePath -> Reporter a -> IO (Maybe a)
-- schreibt HTLM in file
-- malt auch bilder (in extra files)
washer fname r = do
    ( mres , out :: WithHTML IO () ) <- run r
    h <- build_document $ out
    writeFile fname $ show h
    return mres

