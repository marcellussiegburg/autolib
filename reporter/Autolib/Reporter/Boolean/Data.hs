module Reporter.Boolean.Data where

--  $Id$

data Boolean i = Not ( Boolean i )
              | And [ Boolean i ]
	      | Or  [ Boolean i ]
	      | Atomic i

instance Functor Boolean where
    fmap f (Atomic x) = Atomic (f x)
    fmap f (Not x) = Not (fmap f x)
    fmap f (And xs) = And $ map (fmap f) xs
    fmap f (Or xs) = Or $ map (fmap f) xs

-- | compute flat normal form
bin_and :: Boolean i -> Boolean i -> Boolean i 
bin_and (And xs) (And ys) = And $ xs ++ ys
bin_and (And xs) y = And $ xs ++ [y]
bin_and x (And ys) = And $ [x] ++ ys
bin_and x y = And [x, y]

-- | compute flat normal form
bin_or :: Boolean i -> Boolean i -> Boolean i 
bin_or (Or xs) (Or ys) = Or $ xs ++ ys
bin_or (Or xs) y = Or $ xs ++ [y]
bin_or x (Or ys) = Or $ [x] ++ ys
bin_or x y = Or [x, y]


