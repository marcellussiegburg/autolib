-- $Header$

module Exp.Print where

import Exp.Type
import ToDoc

instance Show Exp where 
    show = render . toDoc 

instance ToDoc Exp where toDoc = doc 0

-------------------------------------------------------------------------

oper :: Int -> Int -> String -> Exp -> Exp -> Doc
oper me p op l r 
     = klammer me p 
     $ sep [ doc me l, text op, doc me r ]

noper :: Int -> Int -> Exp -> Exp -> Doc
noper me p l r 
     = klammer me p 
     $ cat [ doc me l, doc me r ]

powered :: Int -> Doc -> Exp -> Doc
powered p e x 
	= klammer 7 p
	$ hcat [ doc 7 x, char '^', e, char ' ' ] 

doc :: Int -> Exp -> Doc

doc p (Ref cs) = text (" " ++ cs ++ " ")
doc p (Letter c) = char c

doc p (Union      l r) = oper 1 p "+"  l r
doc p (Difference l r) = oper 2 p "-"  l r
doc p (SymDiff    l r) = oper 2 p "<>" l r

doc p (Intersection l r) = oper 3 p "&" l r
doc p (Shuffle      l r) = oper 4 p "$" l r
doc p (Right_Quotient     l r) = oper 5 p "/" l r
doc p (Left_Quotient     l r) = oper 5 p "\\" l r
doc p (Dot          l r) = noper 6 p l r

doc p (Star x) = powered p (char '*') x
doc p (Plus x) = powered p (char '+') x
doc p (Power e x) = powered p (integer e) x

klammer :: Int -> Int -> Doc -> Doc
klammer p0 p s = {- nest 4 $ -} if p > p0 then  parens s else s



