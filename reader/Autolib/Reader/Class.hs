{-# OPTIONS -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts #-}

module Autolib.Reader.Class 

( module Autolib.Reader.Class
, module Text.ParserCombinators.Parsec
)

where

--   $Id$

import Text.ParserCombinators.Parsec 
        hiding ( label 
#if (__GLASGOW_HASKELL__ >= 604)
               , State 
#endif
               )

class Reader a where

      reader :: Parser a
      reader = readerPrec 0 -- default

      readerPrec :: Int -> Parser a
      readerPrec p = reader -- default

