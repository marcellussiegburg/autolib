module NFA.Equiv where

-- $Id$

import NFA

import Set
import MapSet
import FiniteMap

type Klassen s = Set (Set s)
type Mappe   s = FiniteMap s Int

type Trenner s = ( s, s, Char )

----------------------------------------------------------------------

toMappe :: Ord s => Klassen s -> Mappe s
toMappe xss = listToFM $ do
    ( k, xs ) <- zip [1 .. ] $ setToList xss
    x <- setToList xs
    return ( x, k )

toKlassen :: Ord s => Mappe s -> Klassen s
toKlassen fm = mkSet $ eltsFM $ addListToFM_C union emptyFM $ do
    ( x, k ) <- fmToList fm
    return ( k, unitSet x )

----------------------------------------------------------------------

refine :: Ord s => (s -> s -> Bool) -> Klassen s -> Klassen s
-- verfeinert äquivalenzklassen
refine eq xss = mkSet $ do xs <- setToList xss 
			   split eq xs 

split :: Ord s => (s -> s -> Bool) -> [s] -> [Set s]
split eq [] = []
split eq (x : xs) =
    let (yeah, noh) = partition (eq x) xs
    in	mkSet yeah : split noh

----------------------------------------------------------------------

trenner :: Ord s => Set Char -> NFA s -> Klassen s -> [ Trenner s ]
-- das berechnet alle
trenner sigma a xss = do
    let fm = toMappe xss
    xs <- setToList xss
    x : ys <- setToList xs
    y <- ys
    c <- setToList sigma
    guard $ not $ equi c a fm x y
    return ( x, y, c )
    
equi :: Ord s => Char -> NFA s -> Mappe s -> (s -> s -> Bool)
equi c a fm p q = and $ do
    -- sollte genau einer sein, wenn a deterministisch und vollst.
    x' <- maybeToList $ lookupFM (trans a) (x,c)
    y' <- maybeToList $ lookupFM (trans a) (y,c)
    return $ lookupFM fm x' == lookupFM fm y'

equivalent :: Ord s => Set Char -> NFA s -> Mappe -> (s -> s -> Bool)
equivalent sigma a fm p q = and $ do
    c <- setToList sigma
    return $ equi c a fm p q

----------------------------------------------------------------------

schritt :: Ord s 
	=> Set Char -> NFA s 
	-> Klassen s -> [ Trenner s ]
	-> Reporter ( Klassen s )
-- prüfe einen schritt
schritt sigma a xss ts = do
    -- sind alle Trenner korrekt
    -- fehlen Trenner?
    
	

    

