module Reader.Class 

( module Reader.Class
, module Text.ParserCombinators.Parsec
)

where

--   $Id$

import Text.ParserCombinators.Parsec hiding ( label )

class Reader a where

      reader :: Parser a
      reader = readerPrec 0 -- default

      readerPrec :: Int -> Parser a
      readerPrec p = reader -- default


