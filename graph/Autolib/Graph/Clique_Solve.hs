module Graph_Clique_Solve where

import Graph
import Graph_Util
import Graph_Clique

import Data.Set
import Autolib.FiniteMap

-------------------------------------------------------------------------------

groessereCliquen :: Ord a => Graph a -> [[a]] -> [[a]]
groessereCliquen g xs =
    setToList ( mkSet( filter ( \l -> length l > length (head xs) )
                              [ setToList (mkSet (l ++ [x]) )
                              | l <- xs, x <- knotenl g, istClique g (l ++ [x])
                              ]
                     )
              )

alleGroesstenCliquen :: Ord a => Graph a ->  [[a]] -> [[a]]
alleGroesstenCliquen g xs =
   let gC = groessereCliquen g xs
   in case gC of
           []  -> xs
           gCl -> alleGroesstenCliquen g gCl
