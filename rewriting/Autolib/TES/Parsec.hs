module TES.Parsec 

( module TES.Parsec
, module Text.ParserCombinators.Parsec
-- , module Text.ParserCombinators.Parsec.Combinator
-- , module Text.ParserCombinators.Parsec.Language
, module Text.ParserCombinators.Parsec.Token
)

where

--   $Id$

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token

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
