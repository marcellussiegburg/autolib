{-# language DeriveDataTypeable #-}

module Autolib.LTL.Data where

import Data.Typeable

data Formula 
    = Variable String
    | Nullary Nop 
    | Unary Uop Formula 
    | Binary Bop Formula Formula
    deriving (Typeable, Eq, Ord )
     
data Nop = Constant Bool
    deriving (Typeable, Eq, Ord )

data Uop = Not
    | Always
    | Eventually
    | Next
    deriving (Typeable, Eq, Ord, Enum, Bounded )

uops :: [ Uop ]
uops = [ minBound .. maxBound ]

data Bop = And | Or | Implies | Until
    deriving (Typeable, Eq, Ord, Enum, Bounded )

bops :: [ Bop ]
bops = [ minBound .. maxBound ]