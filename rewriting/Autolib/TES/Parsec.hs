module TES.Parsec where

-- $Id$

import ParsecLanguage
import ParsecToken

tes = makeTokenParser 
    $ emptyDef
       { commentLine = "#"
       , commentStart = "~"
       , commentEnd = "~"
       , reservedOpNames = [ "->" ]
       }
