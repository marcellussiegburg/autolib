-- -- $Id$

module Exp.Syntax where

import Exp.Type
import Exp.Print

import Reporter
import Sets
import Util.Size
import ToDoc


subtrees :: RX c -> [ RX c ]
subtrees x = x : case x of
 
    Ref c -> []
    Letter c -> []

    Dot l r -> subtrees l ++ subtrees r
    Union l r  -> subtrees l ++ subtrees r
    Intersection l r -> subtrees l ++ subtrees r
    Difference l r -> subtrees l ++ subtrees r
    SymDiff l r -> subtrees l ++ subtrees r
    Shuffle l r -> subtrees l ++ subtrees r
    Left_Quotient l r -> subtrees l ++ subtrees r
    Right_Quotient l r -> subtrees l ++ subtrees r

    Star x -> subtrees x
    Plus x -> subtrees x
    Power i x -> subtrees x

instance Size (RX c) where size = length . subtrees

constructor :: RX c -> String
constructor x = case x of

    Ref n -> "Ref"
    Letter c -> "Letter"

    Dot l r -> "Dot"
    Union l r  -> "Union"
    Intersection l r -> "Intersection"
    Difference l r -> "Difference"
    SymDiff l r -> "SymDiff"
    Shuffle l r -> "Shuffle"

    Star x -> "Star"
    Plus x -> "Plus"
    Power i x -> "Power"


constructors :: Exp  -> Set String 
constructors = 
    mkSet . map constructor . subtrees 


star_height :: (Num a, Ord a) => Exp -> a
star_height x = case x of
    Ref n -> 0 -- gefährlich
    Letter c -> 0

    Dot          l r -> max (star_height l) (star_height r)
    Union        l r -> max (star_height l) (star_height r)
    Intersection l r -> max (star_height l) (star_height r)
    Difference   l r -> max (star_height l) (star_height r)
    SymDiff      l r -> max (star_height l) (star_height r)
    Shuffle      l r -> max (star_height l) (star_height r)

    Star x -> 1 + star_height x
    Plus x -> 1 + star_height x
    Power i x -> star_height x


