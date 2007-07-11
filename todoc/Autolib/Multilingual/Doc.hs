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

import Autolib.Multilingual.Language

import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Data.Map
import Data.Map ( Map )
import Data.List ( nub )

----------------------------------------------------------------
-- Types

type Style = PP.Style

data Doc = Doc 
         { contents :: Map Language PP.Doc 
         }

render = render_for DE
render_for lang = PP.render . specialize lang

instance Show Doc where show = render

----------------------------------------------------------------
-- Accessors, Constructors

specialize :: Language -> Doc -> PP.Doc
specialize lang doc = 
    case Data.Map.lookup lang ( contents doc )  of
        Just this -> this
        Nothing   -> PP.text $ "no output for language " ++ show lang

-- | use default language 
text :: String -> Doc
text cs = multitext [ ( DE, cs )]

-- | use several languages
multitext :: [(Language, String)] -> Doc
multitext = multidoc . map ( \ ( l, cs ) -> ( l, PP.text cs ) )

-- | use several languages
multidoc :: [(Language, PP.Doc)] -> Doc
multidoc arg = Doc 
    { contents = Data.Map.fromList arg }

----------------------------------------------------------------
-- Combinators

fold_unary :: ( PP.Doc -> PP.Doc )
           -> Doc
           -> Doc
fold_unary op x = Doc { contents = Data.Map.map op $ contents x }

fold_binary :: ( PP.Doc -> PP.Doc -> PP.Doc ) 
      -> Doc -> Doc 
      -> Doc
fold_binary op x y =
     Doc { contents = Data.Map.unionWith op ( contents x ) ( contents y ) }

fold_list :: ( [ PP.Doc ] -> PP.Doc ) 
      -> [ Doc ]
      -> Doc
fold_list op xs =
    let get l = do x <- xs ; return $ specialize l x
        ls = nub $ concat $ map ( Data.Map.keys . contents ) xs
    in  multidoc $ do l <- ls ; return ( l, op $ get l )

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

uniform d = multidoc
      $ do l <- [ minBound .. maxBound ] ; return ( l, d )

int = uniform . PP.int
integer = uniform . PP.integer
float = uniform . PP.float
double = uniform . PP.double

empty = uniform PP.empty



