module Reader.Class where

-- -- $Id$

import Parsec

class Reader a where

      reader :: Parser a
      reader = readerPrec 0 -- default

      readerPrec :: Int -> Parser a
      readerPrec p = reader -- default


