module Graph_Hamilton_Solve where

import Graph
import Graph_Util
import Graph_Hamilton

import Set
import FiniteMap

-------------------------------------------------------------------------------

längereWege :: Ord a => Graph a -> [[a]] -> [[a]]
längereWege g xs =
    setToList ( mkSet( filter ( \l -> isEmptySet (mehrfacheKnoten l) )
                              [ l ++ [x]
                              | l <- xs, x <- knotenl g, istWeg g (l ++ [x])
                              ]
                     )
              )

längsteWege :: Ord a => Graph a -> [[a]] -> [[a]]
längsteWege g xs =
   let lW = längereWege g xs
   in case lW of
           []  -> xs
           lWg -> case length (head lWg) == length (knotenl g) of
                  True  -> lWg
                  False -> längsteWege g lWg
 
alleHamiltons :: Ord a => Graph a -> [[a]]
alleHamiltons g = [ h | h <- längsteWege g [[head (knotenl g)]]
                      , istHamilton g h ]