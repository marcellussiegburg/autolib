module Graph_Clique_Solve where

import Graph
import Graph_Util
import Graph_Clique

import Data.Set
import Data.FiniteMap

-------------------------------------------------------------------------------

grössereCliquen :: Ord a => Graph a -> [[a]] -> [[a]]
grössereCliquen g xs =
    setToList ( mkSet( filter ( \l -> length l > length (head xs) )
                              [ setToList (mkSet (l ++ [x]) )
                              | l <- xs, x <- knotenl g, istClique g (l ++ [x])
                              ]
                     )
              )

alleGrösstenCliquen :: Ord a => Graph a ->  [[a]] -> [[a]]
alleGrösstenCliquen g xs =
   let gC = grössereCliquen g xs
   in case gC of
           []  -> xs
           gCl -> alleGrösstenCliquen g gCl
