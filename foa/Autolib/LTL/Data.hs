{-# language DeriveDataTypeable #-}

module Autolib.LTL.Data where

import Autolib.Size
import Autolib.Hash
import Data.Typeable
import Data.List ( nub )
import Control.Monad ( guard )

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

temporal_uops :: Formula -> [ Uop ]
temporal_uops f = nub $ do
    Unary u x <- subtrees f
    guard $ uop_is_temporal u
    return u

instance Size Formula where size = length . subtrees
     
data Nop = Constant Bool
    deriving (Typeable, Eq, Ord )

data Uop = Not
    | Always
    | Eventually
    | Next
    deriving (Typeable, Eq, Ord, Enum, Bounded )

uop_is_temporal u = case u of
    Always -> True
    Eventually -> True
    Next -> True
    _ -> False

temporal_bops :: Formula -> [ Bop ]
temporal_bops f = nub $ do
    Binary b x y <- subtrees f
    guard $ bop_is_temporal b
    return b

uops :: [ Uop ]
uops = [ minBound .. maxBound ]

data Bop = And | Or | Implies | Iff
         | Until
    deriving (Typeable, Eq, Ord, Enum, Bounded )

bop_is_temporal b = case b of
    Until -> True
    _ -> False

bops :: [ Bop ]
bops = [ minBound .. maxBound ]