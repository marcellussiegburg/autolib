-- -- $Id$

module Autolib.Exp.Syntax where

import Autolib.Exp.Type
import Autolib.Exp.Print

import Autolib.Reporter
import Autolib.Set
import Autolib.Util.Size
import Autolib.Hash
import Autolib.ToDoc


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

    PowerStar x -> subtrees x
    PowerPlus x -> subtrees x
    Power i x -> subtrees x

instance Size (RX c) where size = length . subtrees

instance Hash c => Hash ( RX c ) where
    hash = hash . map constructor . subtrees

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

    PowerStar x -> "PowerStar"
    PowerPlus x -> "PowerPlus"
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

    PowerStar x -> 1 + star_height x
    PowerPlus x -> 1 + star_height x
    Power i x -> star_height x


