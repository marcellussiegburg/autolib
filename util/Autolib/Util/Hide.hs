module Util.Hide where

-- -- $Id$

newtype Hide a = Hide { unHide :: a } deriving Show

instance Eq (Hide a) where x == y = True
instance Ord (Hide a) where compare x y = EQ
