{-# language OverloadedStrings #-}
{-# language FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# language GADTs #-}
{-# language Rank2Types #-}

module Autolib.Logic.Formula.FO.Data where

import Autolib.Logic.Formula.Name

import Autolib.Reader


data Form n where
    Var :: n -> Form n -- need this for parsing?

    Succ :: n -> n -> Form n
    Less :: n -> n -> Form n
    Letter :: Name -> n -> Form n
    Not :: Form n -> Form n
    Or :: Form n -> Form n -> Form n
    And :: Form n -> Form n -> Form n
    Implies :: Form n -> Form n -> Form n
    Forall :: (n -> Form n) -> Form n
    Exists :: (n -> Form n) -> Form n

data  Formula = Formula ( forall n . Form n )

f1 :: Formula 
f1 = Formula
   $ Forall $ \ x -> Implies ( Letter "A" x ) 
   $ Exists $ \ y -> And (Succ x y) (Letter "B" y)





