module Reporter.Boolean.Data where

--  $Id$

import Char

-- | unary operator
data Up = Not | Success | Star
     deriving ( Eq, Ord, Show, Enum, Bounded )

uname :: Up -> String
uname = map toLower . show

-- | binary operator
-- warning: the ordering here determines the precedence in parsing
data Bop = Seq | Par | Or | And 
     deriving ( Eq, Ord, Show, Enum, Bounded )

bname :: Bop -> String
bname Seq = "."
bname op = map toLower $ show $ op


data Boolean i = Uf Up ( Boolean i )
              | Bof Bop [ Boolean i ]
	      | Atomic i

instance Functor Boolean where
    fmap f (Atomic x) = Atomic (f x)
    fmap f (Uf up x) = Uf up (fmap f x)
    fmap f (Bof bop xs) = Bof bop $ map (fmap f) xs

-- | hehe
instance Monad Boolean where
    return = Atomic
    -- the following would look better with 'join'
    Atomic x >>= f = f x
    Uf up x  >>= f = Uf up ( x >>= f )
    Bof bop xs >>= f = Bof bop ( map ( >>= f ) xs )

-- | traverse and compute, depth-first
fmapM :: Monad m
      => ( a -> m b ) 
      -> Boolean a 
      -> m ( Boolean b )
fmapM f (Atomic x) = do
    y <- f x
    return $ Atomic y
fmapM f (Uf up x) = do
    y <- fmapM f x
    return $ Uf up y
fmapM f (Bof bop xs) = do
    ys <- mapM ( fmapM f ) xs
    return $ Bof bop ys

transpose :: Monad m
	  => Boolean (m a)
	  -> m (Boolean a)
transpose (Atomic m) = 
    do x <- m ; return $ Atomic x
transpose (Uf up m) = do
    y <- transpose m ; return $ Uf up y
transpose (Bof bop xs) = do
    ys <- mapM transpose xs ; return $ Bof bop ys


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



