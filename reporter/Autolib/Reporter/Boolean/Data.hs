module Reporter.Boolean.Data where

--  $Id$

import Char

-- | unary operator
data Up = Not | Success
     deriving ( Eq, Ord, Show, Enum, Bounded )

uname :: Up -> String
uname = map toLower . show

-- | binary operator
-- warning: the ordering here determines the precedence in parsing
data Bop = Par | Or | And 
     deriving ( Eq, Ord, Show, Enum, Bounded )

bname :: Bop -> String
bname = map toLower . show



data Boolean i = Uf Up ( Boolean i )
              | Bof Bop [ Boolean i ]
	      | Atomic i

instance Functor Boolean where
    fmap f (Atomic x) = Atomic (f x)
    fmap f (Uf up x) = Uf up (fmap f x)
    fmap f (Bof bop xs) = Bof bop $ map (fmap f) xs

-- | compute flat normal form
bin :: Bop -> Boolean i -> Boolean i -> Boolean i 
bin op x @ (Bof fx xs) y @ (Bof fy ys) 
    | op == fx && fx == fy  = Bof op $ xs ++ ys
bin op x @ (Bof fx xs) y 
    | op == fx              = Bof op $ xs ++ [y] 
bin op x y @ (Bof fy ys) 
    | op == fy		    = Bof op $ [x] ++ ys
bin op x y 
    | otherwise             = Bof op [ x, y ]



