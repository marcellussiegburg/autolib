module Graph_Hamilton_Solve where

import Graph
import Graph_Util
import Graph_Hamilton

import Set
import FiniteMap

-------------------------------------------------------------------------------

l�ngereWege :: Ord a => Graph a -> [[a]] -> [[a]]
l�ngereWege g xs =
    setToList ( mkSet( filter ( \l -> isEmptySet (mehrfacheKnoten l) )
                              [ l ++ [x]
                              | l <- xs, x <- knotenl g, istWeg g (l ++ [x])
                              ]
                     )
              )

l�ngsteWege :: Ord a => Graph a -> [[a]] -> [[a]]
l�ngsteWege g xs =
   let lW = l�ngereWege g xs
   in case lW of
           []  -> xs
           lWg -> case length (head lWg) == length (knotenl g) of
                  True  -> lWg
                  False -> l�ngsteWege g lWg
 
alleHamiltons :: Ord a => Graph a -> [[a]]
alleHamiltons g = [ h | h <- l�ngsteWege g [[head (knotenl g)]]
                      , istHamilton g h ]