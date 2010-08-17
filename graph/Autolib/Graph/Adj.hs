-- | Adjazenzmatrizen

module Autolib.Graph.Adj where

import Autolib.Graph.Type ( Graph, GraphC , knoten , kanten , kante , mkGraph )
import Autolib.Set ( mkSet , cardinality , setToList , elementOf )

import Data.List ( groupBy , intersperse , transpose )
import Data.Array ( Array , array , bounds , assocs , listArray , (//) , (!) )

import Control.Monad ( guard , liftM )

import Autolib.FiniteMap ( listToFM , lookupWithDefaultFM )

-------------------------------------------------------------------------------

type Matrix a = Array (Int,Int) a
type AdjMatrix = Matrix Integer

-------------------------------------------------------------------------------

adjazenz_matrix :: GraphC a => Graph a -> AdjMatrix
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

from_adjazenz_matrix :: GraphC Int => AdjMatrix -> Graph Int
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

-- | wegematrix naiv via horner schema: O(n^4)
wegematrix :: GraphC a => Graph a -> AdjMatrix
wegematrix g = let a = adjazenz_matrix g
		   n = cardinality $ knoten g
	       in sig $ foldr ( \ v m -> add v $ prod v m ) a 
		      $ replicate (pred n) a

-- | wegematrix per algorithmus von warshall: O(n^3)
warshall :: GraphC a => Graph a -> AdjMatrix
warshall = generic_floyd_warshall update
    where update m (k,i,j)
	    | and [ m ! (i,k) == 1 , m ! (k,j) == 1 ] = m // [((i,j),1)]
	    | otherwise                               = m

-- | matrix der länge der kürzesten wege via floyd-warshall: O(n^3)
floyd_warshall :: GraphC a => Graph a -> AdjMatrix
floyd_warshall = generic_floyd_warshall update
    where update m (k,i,j)
	    | m!(i,k) == 0                = m
	    | m!(k,j) == 0                = m
	    | m!(i,j) == 0                = m // [((i,j),m!(i,k) + m!(k,j))]
	    | m!(i,j) > m!(i,k) + m!(k,j) = m // [((i,j),m!(i,k) + m!(k,j))]
	    | otherwise                   = m

rad_diam :: GraphC a => Graph a -> Maybe (Integer,Integer)
rad_diam g = let ms = map maximum $ zeilen $ floyd_warshall g
                 r = minimum ms
		 d = maximum ms
             in case r of 0 -> Nothing ; _ -> Just (r,d)

rad, diam :: GraphC a => Graph a -> Maybe Integer
rad = liftM fst . rad_diam
diam = liftM snd . rad_diam

-------------------------------------------------------------------------------

generic_floyd_warshall :: GraphC a 
    => ( AdjMatrix -> (Int,Int,Int) -> AdjMatrix ) -> Graph a -> AdjMatrix
generic_floyd_warshall update g = 
    let a = adjazenz_matrix g
	n = size a
    in foldl update a $ do k <- [1..n]
			   i <- [1..n]
			   j <- [1..n]
			   return (k,i,j)

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
