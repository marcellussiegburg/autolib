-- | Adjazenzmatrizen

module Autolib.Graph.Adj where

import Autolib.Graph.Type ( Graph , knoten , kanten , kante , mkGraph )
import Autolib.Set ( mkSet , cardinality , setToList , elementOf )

import Data.List ( groupBy , intersperse )
import Data.Array ( Array , array , bounds , assocs )

import Control.Monad ( guard )

import FiniteMap ( listToFM , lookupWithDefaultFM )

-------------------------------------------------------------------------------

type AdjMatrix = Array (Int,Int) Bool

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
       return ((i,j),kante x y `elementOf` (kanten g))

-------------------------------------------------------------------------------

from_adjazenz_matrix :: AdjMatrix -> Graph Int
from_adjazenz_matrix arr = 
    let n = snd $ snd $ bounds arr
    in mkGraph (mkSet [1..n])
               (mkSet $ do
		((i,j),inzident) <- assocs arr
		guard inzident
		return $ kante i j
	       )

-------------------------------------------------------------------------------

zeilen :: AdjMatrix -> [[Bool]]
zeilen = map ( map snd ) . groupBy ff . assocs
    where ff ((x,_),_) ((y,_),_) = x == y

schoen ::AdjMatrix -> String
schoen = unlines . map ( concat . intersperse " " . map bshow ) . zeilen
    where bshow True = "1"
	  bshow _    = "0"
