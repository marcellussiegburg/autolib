-- $Header$

module Exp.MyTokens where

import Parsec
import ParsecLanguage
import qualified ParsecToken as P

me            = P.makeTokenParser meDef

lexeme          = P.lexeme me
parens          = P.parens me    
braces          = P.braces me    
semiSep         = P.semiSep me    
semiSep1        = P.semiSep1 me    
commaSep        = P.commaSep me    
commaSep1       = P.commaSep1 me    
whiteSpace      = P.whiteSpace me    
symbol          = P.symbol me    
identifier      = P.identifier me    
reserved        = P.reserved me    
reservedOp      = P.reservedOp me    
natural         = P.natural me    

meDef
    = haskellStyle
        { identStart        = letter
        , identLetter       = letter
        , opStart           = opLetter meDef
        , opLetter          = oneOf ":=\\/<>|~.*-+$^;"
        , reservedOpNames   = [ "+", "-", "&", "$", "^*", "^+", ";" ]
        , reservedNames     = [ "let" ] 
        }

