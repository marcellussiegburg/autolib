module TES.Parsec where

-- $Id$

import ParsecLanguage
import ParsecToken

tes_prefix = makeTokenParser 
    $ emptyDef
       { commentLine = "#" 
       , commentStart = "(*"
       , commentEnd = "*)"
       , reservedOpNames = [ "->", ";" ]
       }

tes = makeTokenParser 
    $ emptyDef
       { commentLine = "" 
       , commentStart = ""
       , commentEnd = ""
       , reservedNames = [ "COMMENT" ]
       , reservedOpNames = [ "->", ";", ".", "~" ]
       }
