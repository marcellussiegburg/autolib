{-# language DeriveDataTypeable #-}

module Autolib.LTL.Data where

import Autolib.Size
import Autolib.Hash
import Data.Typeable
import Data.List ( nub )

newtype Name = Name String
    deriving ( Eq, Ord )

instance Hash Name where hash (Name s) = hash s

data Formula 
    = Variable Name
    | Nullary Nop 
    | Unary Uop Formula 
    | Binary Bop Formula Formula
    deriving (Typeable, Eq, Ord )

subtrees :: Formula -> [ Formula ]
subtrees f = f : case f of
    Unary _ g -> subtrees g
    Binary _ g h -> [g,h] >>= subtrees
    _ -> []

variables :: Formula -> [ Name ]
variables f = nub $ do 
    Variable n <- subtrees f ; return n

instance Size Formula where size = length . subtrees
     
data Nop = Constant Bool
    deriving (Typeable, Eq, Ord )

data Uop = Not
    | Always
    | Eventually
    | Next
    deriving (Typeable, Eq, Ord, Enum, Bounded )

uops :: [ Uop ]
uops = [ minBound .. maxBound ]

data Bop = And | Or | Implies | Iff
         | Until
    deriving (Typeable, Eq, Ord, Enum, Bounded )

bops :: [ Bop ]
bops = [ minBound .. maxBound ]