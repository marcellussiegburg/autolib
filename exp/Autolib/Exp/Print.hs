-- -- $Id$

module Autolib.Exp.Print where

import Autolib.Symbol
import Autolib.Exp.Type
import Autolib.ToDoc

instance Symbol c => Show ( RX c ) where 
    show = render . toDoc 

instance Symbol c => ToDoc ( RX c ) where toDoc = doc 0

-------------------------------------------------------------------------

oper :: ( Symbol c ) =>  Int -> Int -> String -> (RX c) -> (RX c) -> Doc
oper me p op l r 
     = klammer me p 
     $ sep [ doc me l, text op, doc me r ]

noper :: ( Symbol c ) =>  ( [ Doc ] -> Doc ) 
      -> Int -> Int -> (RX c) -> (RX c) -> Doc
noper glue me p l r 
     = klammer me p 
     $ glue [ doc me l, doc me r ]

powered :: ( Symbol c ) =>  Int -> Doc -> (RX c) -> Doc
powered p e x 
	= klammer 7 p
	$ hcat [ doc 7 x, char '^', e ] 

doc :: ( Symbol c ) =>  Int -> (RX c) -> Doc

doc p (Ref cs) = text cs
doc p (Letter c) = symbol_toDoc c

doc p (Union      l r) = oper 1 p "+"  l r
doc p (Difference l r) = oper 2 p "-"  l r
doc p (SymDiff    l r) = oper 2 p "<>" l r

doc p (Intersection l r) = oper 3 p "&" l r
doc p (Shuffle      l r) = oper 4 p "$" l r
doc p (Right_Quotient     l r) = oper 5 p "/" l r
doc p (Left_Quotient     l r) = oper 5 p "\\" l r
doc p x @ (Dot          l r) =
    let glue = if istwort x then cat else sep
    in noper glue 6 p l r

doc p (PowerStar x) = powered p (char '*') x
doc p (PowerPlus x) = powered p (char '+') x
doc p (PowerOmega x) = powered p (text "omega") x
doc p (Power e x) = powered p (integer e) x

klammer :: Int -> Int -> Doc -> Doc
klammer p0 p s = {- nest 4 $ -} if p > p0 then  parens s else s

istwort :: ( Symbol c ) =>  (RX c) -> Bool
istwort (Letter c) = True
istwort (Dot l r) = istwort l && istwort r
istwort _ = False

