module Util.Hide where

newtype Hide a = Hide a deriving Show; unHide (Hide x) = x
instance Eq (Hide a) where x == y = True
instance Ord (Hide a) where compare x y = EQ
