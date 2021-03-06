-- | Halbordnung

module Autolib.Partial where

import Autolib.Set
import Control.Monad ( guard )
import Data.List ( isPrefixOf )

class Partial a where
      -- | eine von beiden methoden muß implementiert werden
      leq :: a -> a -> Bool
      leq x y = geq y x

      geq :: a -> a -> Bool
      geq y x = leq x y


lt :: Partial a => a -> a -> Bool
lt x y = leq x y && not (geq x y)

gt :: Partial a => a -> a -> Bool
gt y x = lt x y


-- | minimale elemente
mins :: Partial a => [a] -> [a]
mins xs = do
    x <- xs
    guard $ not $ any ( \ y -> lt y x ) xs
    return x

-- | maximale elemente
maxs :: Partial a => [a] -> [a]
maxs xs = do
    x <- xs
    guard $ not $ any ( \ y -> gt y x ) xs
    return x

-- | extensions to sets (siehe dieters einleitung)
instance Partial a => Partial (Set a) where
    leq xs ys = and $ do
	x <- setToList xs
	return $ or $ do 
	    y <- setToList ys
	    return $ leq x y

-- | prefix-ordnung (ohne vergleich der elemente)
-- ist natürlich wohlfundiert
instance Eq a => Partial [a] where leq = isPrefixOf

-- | ein paar einfache instanzen, damit wir beispiele haben
instance Ord a => Partial a where geq = (>=)

 