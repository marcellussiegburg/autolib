{-# language OverloadedStrings #-}
{-# language FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# language GADTs #-}
{-# language Rank2Types #-}
{-# language DeriveDataTypeable #-}

module Autolib.Logic.Formula.FO.Data where

import Autolib.Logic.Formula.Name

import Autolib.Reader
import Autolib.Size

import Data.Typeable

data Form n where
    Succ :: n -> n -> Form n
    Less :: n -> n -> Form n
    Letter :: Char -> n -> Form n
    Not :: Form n -> Form n
    Or :: Form n -> Form n -> Form n
    And :: Form n -> Form n -> Form n
    Implies :: Form n -> Form n -> Form n
    Forall :: (n -> Form n) -> Form n
    Exists :: (n -> Form n) -> Form n

data  Formula = Formula ( forall n . Form n )
    deriving Typeable

f1 :: Formula 
f1 = Formula
   $ Forall $ \ x -> Implies ( Letter 'A' x ) 
   $ Exists $ \ y -> And (Succ x y) (Letter 'B' y)

fsize :: Form n -> Int
fsize f = case f of
    Succ {} -> 1 ; Less {} -> 1 ; Letter {} -> 1
    Not g -> 1 + fsize g ; Or g h -> 1 + fsize g + fsize h
    And g h -> 1 + fsize g + fsize h ; Implies g h -> 1 + fsize g + fsize h
    Forall u -> 1 + fsize ( u undefined )
    Exists u -> 1 + fsize ( u undefined )

instance Size Formula where size (Formula f) = fsize f

