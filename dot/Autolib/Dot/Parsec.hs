module Dot.Parsec where

-- $Id$

import Reader
import Parsec

soi :: Parser String           
soi =   do xs <- many1 alphaNum ; spaces ; return xs 
    <|> my_stringLiteral
