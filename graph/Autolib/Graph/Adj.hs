-- | Adjazenzmatrizen

module Autolib.Graph.Adj where

import Autolib.Graph.Type ( Graph , knoten , kanten , kante , mkGraph )
import Autolib.Set ( mkSet , cardinality , setToList , elementOf )

import Data.List ( groupBy , intersperse , transpose )
import Data.Array ( Array , array , bounds , assocs , listArray )

import Control.Monad ( guard )

import Autolib.FiniteMap ( listToFM , lookupWithDefaultFM )

-------------------------------------------------------------------------------

type Matrix a = Array (Int,Int) a
type AdjMatrix = Matrix Integer

-------------------------------------------------------------------------------

adjazenz_matrix :: Ord a => Graph a -> AdjMatrix
adjazenz_matrix g = 
    let n = cardinality $ knoten g
	m = listToFM $ zip [(1::Int)..] $ setToList $ knoten g
	kn = lookupWithDefaultFM m ( error "Autolib.Graph.Adj.kn" )
    in array ((1,1),(n,n)) $ do
       i <- [1..n]
       j <- [1..n]
       let (x,y) = (kn i,kn j)
       return ((i,j),if kante x y `elementOf` (kanten g) then 1 else 0)

-------------------------------------------------------------------------------

from_adjazenz_matrix :: AdjMatrix -> Graph Int
from_adjazenz_matrix m = 
    let n = size m
    in mkGraph (mkSet [1..n])
               (mkSet $ do
		((i,j),inzident) <- assocs m
		guard $ inzident == 1
		return $ kante i j
	       )

-------------------------------------------------------------------------------

{- specification:
wegematrix :: Ord a => Graph a -> AdjMatrix
wegematrix g = let m = adjazenz_matrix g
	       in sig $ foldl1 add $ do
	          i <- [ 1 .. cardinality (knoten g) ]
		  return $ pow i m
-}

-- | faster via horner schema
wegematrix :: Ord a => Graph a -> AdjMatrix
wegematrix g = let a = adjazenz_matrix g
		   n = cardinality $ knoten g
	       in sig $ foldr ( \ v m -> add v $ prod v m ) a 
		      $ replicate (pred n) a

-------------------------------------------------------------------------------

zeilen :: Matrix a -> [[a]]
zeilen = map ( map snd ) . groupBy ff . assocs
    where ff ((x,_),_) ((y,_),_) = x == y

spalten :: Matrix a -> [[a]]
spalten = transpose . zeilen

sig :: ( Ord a , Num a ) => Matrix a -> Matrix Integer
sig m = array (bounds m) $ map adjust $ assocs m
    where adjust (i,v) | v > 0     = (i,1)
		       | otherwise = (i,0)

size :: Matrix a -> Int
size = fst . snd . bounds

unit :: Num a => Int -> Matrix a
unit n = listArray ((1,1),(n,n)) $ do
	 (i,j) <- cross1 [1..n]
	 return $ if i == j then 1 else 0

cross1 :: [a] -> [(a,a)]
cross1 xs = do x <- xs ; y <- xs ; return (x,y)

pow :: Num a => Int -> Matrix a -> Matrix a
pow 0 m = unit $ size m
pow 1 m = m
pow k m = prod m $ pow (pred k) m

prod :: Num a => Matrix a -> Matrix a -> Matrix a
prod a b = let zs = zeilen a
	       sp = spalten b
           in listArray (bounds a) 
	      $ concat
	      $ map ( \ z -> map sum $ zipWith (zipWith (*)) (repeat z) sp ) zs

add :: Num a => Matrix a -> Matrix a -> Matrix a
add a b = array (bounds a) 
	$ zipWith ( \ (i,x) (_,y) -> (i,x+y) ) (assocs a) (assocs b)

-------------------------------------------------------------------------------

schoen :: Show a => Matrix a -> String
schoen = unlines . map ( concat . intersperse " " . map show ) . zeilen
