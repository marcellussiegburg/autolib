module Graph_Clique_Solve where

import Graph
import Graph_Util
import Graph_Clique

import Data.Set
import Data.FiniteMap

-------------------------------------------------------------------------------

gr�ssereCliquen :: Ord a => Graph a -> [[a]] -> [[a]]
gr�ssereCliquen g xs =
    setToList ( mkSet( filter ( \l -> length l > length (head xs) )
                              [ setToList (mkSet (l ++ [x]) )
                              | l <- xs, x <- knotenl g, istClique g (l ++ [x])
                              ]
                     )
              )

alleGr�sstenCliquen :: Ord a => Graph a ->  [[a]] -> [[a]]
alleGr�sstenCliquen g xs =
   let gC = gr�ssereCliquen g xs
   in case gC of
           []  -> xs
           gCl -> alleGr�sstenCliquen g gCl
