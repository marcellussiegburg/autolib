module Graph_Hamilton_Solve where

import Graph
import Graph_Util
import Graph_Hamilton

import Data.Set
import Data.FiniteMap

-------------------------------------------------------------------------------

laengereWege :: Ord a => Graph a -> [[a]] -> [[a]]
laengereWege g xs =
    setToList ( mkSet( filter ( \l -> isEmptySet (mehrfacheKnoten l) )
                              [ l ++ [x]
                              | l <- xs, x <- knotenl g, istWeg g (l ++ [x])
                              ]
                     )
              )

laengsteWege :: Ord a => Graph a -> [[a]] -> [[a]]
laengsteWege g xs =
   let lW = laengereWege g xs
   in case lW of
           []  -> xs
           lWg -> case length (head lWg) == length (knotenl g) of
                  True  -> lWg
                  False -> laengsteWege g lWg
 
alleHamiltons :: Ord a => Graph a -> [[a]]
alleHamiltons g = [ h | h <- laengsteWege g [[head (knotenl g)]]
                      , istHamilton g h ]