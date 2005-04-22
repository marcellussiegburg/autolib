{-# OPTIONS -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts #-}

module Autolib.Reader.Class 

( module Autolib.Reader.Class
, module Text.ParserCombinators.Parsec
, guard
)

where

--   $Id$

import Text.ParserCombinators.Parsec 
        hiding ( label 
#if (__GLASGOW_HASKELL__ >= 604)
               , State 
#endif
               )

import Control.Monad ( guard )
import Autolib.Reader.Basic ( my_parens )
import Text.ParserCombinators.Parsec.Token ( whiteSpace )
import Text.ParserCombinators.Parsec.Language ( haskell )

-- | @atomic_reader@ or @atomic_readerPrec@ must be implemented.
-- it can start parsing right away.
-- from the outside, you should call @reader@
-- which allows enclosing parentheses.
-- to require enclosing parentheses, 
-- explicitely use @reader_Paren True@

class Reader a where

      readerPrec :: Int -> Parser a
      readerPrec d = readerParenPrec d atomic_readerPrec

      reader :: Parser a
      reader = readerPrec 0

      atomic_reader :: Parser a
      atomic_reader = atomic_readerPrec 0 -- default

      atomic_readerPrec :: Int -> Parser a
      atomic_readerPrec d = atomic_reader -- default

-- | read with enclosing parens
readerParenPrec :: Int -> ( Int -> Parser a ) -> Parser a
readerParenPrec d pp
    = try ( pp d )
    <|> my_parens ( readerParenPrec 0 pp )

parse_complete :: Parser p -> Parser p
parse_complete p = do 
    whiteSpace haskell
    x <- p
    eof
    return x

