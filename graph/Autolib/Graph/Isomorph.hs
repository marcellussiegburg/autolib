-- | vergleicht zwei graphen auf isomorphie

-- autor m.lindemeyer
-- stinfwww.informatik.uni-leipzig.de/~psy99hvr
-- (8484955)

module Autolib.Graph.Isomorph (isIsomorph) where 

import Autolib.Graph.Graph
import Autolib.Util.Sort
import Data.Set
import Autolib.Graph.Util
import Autolib.Graph.Beispiele

---------------------------------------------------------------------------------------------------
-- | prueft die Isomorphie 
isIsomorph :: ( GraphC a, GraphC b ) => Graph a -> Graph b -> Bool
isIsomorph a b = case and[knotensetId a b, kantensetId a b] of
			True -> isomorphgraph a b 
			False -> False

-- | vergleicht zwei graphen die nach knotenanzahl und kantenanzahl identisch sind
isomorphgraph :: ( GraphC a, GraphC b   ) => Graph a -> Graph b -> Bool
isomorphgraph a b = 
  if (sort(gradliste a))==(sort(gradliste b)) 
    then (sort(grad2liste a))==(sort(grad2liste b))
    else False

---------------------------------------------
-- | gradzahlen jeden knotens in eine liste ohne sortierung

gradliste :: ( GraphC a ) => Graph a -> [Int]
gradliste a = do
              x <- (knotenliste a)
              return (grad a x)

---------------------------------------------
-- | gradzahlen der gradzahlen jeden knotens in eine liste ohne sortierung

grad2liste :: ( GraphC a ) => Graph a -> [[Int]]
grad2liste a = do
                x <- (knotenliste a)
                return (sort(grad2 a x))
