module Autolib.Dot.Parsec where

-- -- $Id$

import Autolib.Reader
import Text.ParserCombinators.Parsec

soi :: Parser String           
soi =   do xs <- many1 alphaNum ; spaces ; return xs 
    <|> my_stringLiteral
