{-# OPTIONS -fallow-overlapping-instances -fallow-undecidable-instances -fglasgow-exts #-}

module Autolib.Reader.Link where

--  $Id$

import Autolib.Reader.Class
import Autolib.Reader.Basic

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec.Language ( haskell )

import Control.Monad

instance Reader a => Read a where
    readsPrec = parsec_readsPrec

-- | note: cannot raise an exception here
-- since we might be called from a classical Read parser
-- that wants to parse a list element. this breaks for parsing the empty list
-- curiously, it worked for non-empty lists
parsec_readsPrec :: Reader a => Int -> ReadS a
parsec_readsPrec p input = 
    case parse ( parsec_wrapper p ) "input" input 
    of Right (x, rest) -> return (x, rest)
       Left  err       -> -- error ("\n" ++ input ++ "\n" ++ show err)
                          mzero

parsec_wrapper :: Reader a => Int -> Parser (a, String)
parsec_wrapper p = do 
    whiteSpace haskell
    x <- readerPrec p
    rest <- getInput
    return (x , rest)


