-- | vergleicht zweier graphen auf isomorphie
--
-- autor m.lindemeyer
-- (8484955)

module Graph.Isomorph (isIsomorph) where 

import Graph.Graph
import Sort
import Set
import Graph.Util
-- import Graph.Beispiele

---------------------------------------------------------------------------------------------------
-- | prueft die Isomorphie 
isIsomorph :: (Ord a, Eq a) => Graph a -> Graph a -> Bool
isIsomorph a b = case and[knotensetId a b, kantensetId a b] of
			True -> isomorphgraph a b 
			False -> False

-- | vergleicht zwei graphen die nach knotenanzahl und kantenanzahl identisch sind
isomorphgraph :: (Ord a, Eq a) => Graph a -> Graph a -> Bool
isomorphgraph a b = 
  if (sort(gradliste a))==(sort(gradliste b)) 
    then (sort(grad2liste a))==(sort(grad2liste b))
    else False

---------------------------------------------
-- | gradzahlen jeden knotens in eine liste ohne sortierung

gradliste :: (Ord a, Eq a) => Graph a -> [Int]
gradliste a = do
              x <- (knotenliste a)
              return (grad a x)

---------------------------------------------
-- | gradzahlen der gradzahlen jeden knotens in eine liste ohne sortierung

grad2liste :: (Ord a, Eq a) => Graph a -> [[Int]]
grad2liste a = do
                x <- (knotenliste a)
                return (sort(grad2 a x))
