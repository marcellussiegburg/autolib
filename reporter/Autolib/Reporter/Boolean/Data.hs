module Reporter.Boolean.Data where

--  $Id$

import Char

-- | warning: the ordering here determines the precedence in parsing
data Oper = First | Or | And 
     deriving ( Eq, Ord, Show, Enum, Bounded )

name :: Oper -> String
name = map toLower . show

data Boolean i = Not ( Boolean i )
              | Fun Oper [ Boolean i ]
	      | Atomic i

instance Functor Boolean where
    fmap f (Atomic x) = Atomic (f x)
    fmap f (Not x) = Not (fmap f x)
    fmap f (Fun op xs) = Fun op $ map (fmap f) xs

-- | compute flat normal form
bin :: Oper -> Boolean i -> Boolean i -> Boolean i 
bin op x @ (Fun fx xs) y @ (Fun fy ys) 
    | op == fx && fx == fy  = Fun op $ xs ++ ys
bin op x @ (Fun fx xs) y 
    | op == fx              = Fun op $ xs ++ [y] 
bin op x y @ (Fun fy ys) 
    | op == fy		    = Fun op $ [x] ++ ys
bin op x y 
    | otherwise             = Fun op [ x, y ]



