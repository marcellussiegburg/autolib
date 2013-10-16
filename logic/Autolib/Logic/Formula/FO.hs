{-# language OverloadedStrings #-}
{-# language FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# language GADTs #-}
{-# language Rank2Types #-}

module Autolib.Logic.Formula.FO where

import Autolib.Logic.Formula.Doc () -- instance
import Autolib.Logic.Formula.Name

import Autolib.ToDoc
import Autolib.Reader
import Data.String

data Form n where
    Succ :: n -> n -> Form n
    Less :: n -> n -> Form n
    Apply :: Name -> n -> Form n
    Not :: Form n -> Form n
    Or :: Form n -> Form n -> Form n
    And :: Form n -> Form n -> Form n
    Implies :: Form n -> Form n -> Form n
    Forall :: (n -> Form n) -> Form n
    Exists :: (n -> Form n) -> Form n

data  Formula = Formula ( forall n . Form n )

f1 :: Formula 
f1 = Formula
   $ Forall $ \ x -> Implies ( Apply "A" x ) 
   $ Exists $ \ y -> And (Succ x y) (Apply "Z" y)


instance ToDoc Formula where 
    toDocPrec p (Formula f) = formula names p f
instance Show Formula where 
    show = render . toDoc


formula :: ToDoc n => [ n ] -> Int -> Form n -> Doc
formula names p f = case f of
    Succ l r -> 
        parens $ hsep [ toDoc l, "<1", toDoc r ]
    Less l r -> 
        parens $ hsep [ toDoc l, "<", toDoc r ]
    Apply s f -> 
        hsep [ toDoc s, parens $ toDoc f ]

    Not f -> 
        hsep [ "not" , parens $ formula names p f ]
    And l r -> parens $ hsep 
        [ formula names p l, "&&", formula names p r ]
    Or  l r -> parens $ hsep 
        [ formula names p l, "||", formula names p r ]
    Implies l r -> parens $ hsep 
        [ formula names p l, "=>", formula names p r ]

    Forall f -> with_name names $ \ n names' -> 
             hsep [ "forall" , toDoc n, parens $ formula names' p $ f n ]
    Exists f -> with_name names $ \ n names' -> 
             hsep [ "exists" , toDoc n, parens $ formula names' p $ f n ]

-- | these are used only for printing (?)

names :: [ Name ]
names = [ "p", "q", "r", "s", "t" ] 
        ++ map ( \ i -> Name ("t" ++ show i) ) [2 .. ]

with_name :: [a] -> ( a -> [a] -> b ) -> b
with_name ns cont = cont ( head ns ) ( tail ns )





