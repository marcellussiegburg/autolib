{-# OPTIONS -fglasgow-exts #-}

module Autolib.Multilingual.Doc 

( Doc
, Style
, render, render_for
, text, multitext
, vcat, hcat, cat, fsep, hsep, sep, empty
, nest, parens, brackets, braces
, char, int, integer, double, float
, (<+>), (<>), ($$), ($+$)
, punctuate, doubleQuotes
, equals, comma, colon, semi
)


where

import Autolib.Multilingual

import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Data.Map
import Data.Map ( Map )
import Data.List ( nub )

----------------------------------------------------------------
-- Types

type Style = PP.Style

type Doc = Autolib.Multilingual.Type PP.Doc

render = render_for DE
render_for lang = PP.render . specialize lang

instance Show Doc where show = render

----------------------------------------------------------------
-- Accessors, Constructors

-- | use default language 
text :: String -> Doc
text cs = multitext [ ( DE, cs )]

-- | use several languages
multitext :: [(Language, String)] -> Doc
multitext = Autolib.Multilingual.make 
          . map ( \ ( l, cs ) -> ( l, PP.text cs ) )

(<+>) = fold_binary (PP.<+>)
(<>) = fold_binary (PP.<>)
($$) = fold_binary (PP.$$)
($+$) = fold_binary (PP.$+$)

vcat = fold_list PP.vcat
hcat = fold_list PP.hcat
cat = fold_list PP.cat

fsep = fold_list PP.fsep
hsep = fold_list PP.hsep
sep = fold_list PP.sep

nest d = fold_unary ( PP.nest d )
parens = fold_unary PP.parens
brackets = fold_unary PP.brackets
braces = fold_unary PP.braces
doubleQuotes = fold_unary PP.doubleQuotes

punctuate x [] = []
punctuate x [y] = [y]
punctuate x (y : ys) = y <> x : punctuate x ys
 
comma = uniform PP.comma
colon = uniform PP.colon
semi = uniform PP.semi
equals = uniform PP.equals
char c = uniform ( PP.char c )

int = uniform . PP.int
integer = uniform . PP.integer
float = uniform . PP.float
double = uniform . PP.double

empty = uniform PP.empty



