{-# language OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# language GADTs #-}

module Autolib.Logic.Formula.SO where

import Autolib.Logic.Formula.Doc () -- instance
import Autolib.Logic.Formula.Name

import Autolib.ToDoc
import Autolib.Reader

-- | TODO : use PHOAS (as in FO.hs)
data Formula where
    Succ :: Int -> Int -> Formula 
    Less :: Int -> Int -> Formula
    
    Letter :: Char -> Int -> Formula
    Singleton :: Int -> Formula

    Not :: Formula -> Formula
    Or :: Formula -> Formula -> Formula
    And :: Formula -> Formula -> Formula
    Implies :: Formula -> Formula -> Formula

    Forall :: (Int -> Formula) -> Formula
    Exists :: (Int -> Formula) -> Formula

instance ToDoc Formula where 
    toDocPrec p f = formula names p f
instance Show Formula where 
    show = render . toDoc

formula :: [Int] -> Int -> Formula -> Doc
formula names p f = case f of
    Succ l r -> parens $ hsep [ toDoc l, "<1", toDoc r ]
    Less l r -> parens $ hsep [ toDoc l, "<", toDoc r ]

    Letter c r -> parens $ hsep [ "Letter", text [c], toDoc r ]
    Singleton l -> parens $ hsep [ "Singleton", toDoc l ]

    Not f -> hsep [ "not" , parens $ formula names p f ]
    And l r -> parens $ hsep [ formula names p l, "&&", formula names p r ]
    Or  l r -> parens $ hsep [ formula names p l, "||", formula names p r ]
    Implies l r -> parens $ hsep [ formula names p l, "=>", formula names p r ]

    Forall f -> with_name names $ \ n names' -> 
             hsep [ "forall" , toDoc n, parens $ formula names' p $ f n ]
    Exists f -> with_name names $ \ n names' -> 
             hsep [ "exists" , toDoc n, parens $ formula names' p $ f n ]

-- | these are used only for printing (?)

names :: [ Int ]
names = [ 0 .. ]

with_name :: [a] -> ( a -> [a] -> b ) -> b
with_name ns cont = cont ( head ns ) ( tail ns )





