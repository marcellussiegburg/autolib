-- | A very simple approach for building user interfaces
-- for web apps. Lets you write code like this:
-- 
-- > import Autolib.CGI
-- > import Happstack.Server
-- >
-- > main = simpleHTTP nullConf $ render menu_test
-- >
-- > menu_test = do
-- >    h3 "menu_test"
-- >    (n,c) <- table $ do
-- >        n <- menu "pick a number" [ ("1", 1), ("2", 2), ("3", 3) ]
-- >        c <- menu "pick a letter" [ ("a", 'a'), ("b", 'b'), ("c", 'c') ]
-- >        return (n,c)
-- >   text $ "combined selection:" ++ show (n,c) 


module Autolib.CGI 

( module Autolib.CGI.Widget
)

where

import Autolib.CGI.Widget

