module Main where

import Graph
import Graph_Util
import Graph_Hamilton
import Graph_Hamilton_Solve

import Data.Set
import Autolib.FiniteMap

main = do 
     putStrLn $ unlines 
              $ map ( \g -> show (alleHamiltons g))
                    [ham1,ham2]
--                     ,(symmetrisch (kreis_auf [1..20]))
--                     [entferneKante (symmetrisch (clique_auf [1..10])) 9 10]

ham1 = symmetrisch
       Graph { tafel  = listToFM [ (i, kn i (show i)) | i<-[1..30] ]
             , kanten = mkSet [ ka 6 3
                              , ka 7 4
                              , ka 8 1
                              , ka 8 5
                              , ka 9 2
                              , ka 10 3
                              , ka 11 2
                              , ka 11 8
                              , ka 12 1
                              , ka 12 3
                              , ka 12 9
                              , ka 13 2
                              , ka 13 4
                              , ka 13 6
                              , ka 13 10
                              , ka 14 3
                              , ka 14 5
                              , ka 14 7
                              , ka 15 4
                              , ka 15 8
                              , ka 16 7
                              , ka 16 13
                              , ka 17 6
                              , ka 17 8
                              , ka 17 14
                              , ka 18 7
                              , ka 18 9
                              , ka 18 11
                              , ka 18 15
                              , ka 19 8
                              , ka 19 10
                              , ka 19 12
                              , ka 20 9
                              , ka 20 13
                              , ka 21 12
                              , ka 21 18
                              , ka 22 11
                              , ka 22 13
                              , ka 22 19
                              , ka 23 12
                              , ka 23 14
                              , ka 23 16
                              , ka 23 20
                              , ka 24 13
                              , ka 24 15
                              , ka 24 17
                              , ka 25 14
                              , ka 25 18
                              , ka 26 17
                              , ka 26 23
                              , ka 27 16
                              , ka 27 18
                              , ka 27 24
                              , ka 28 17
                              , ka 28 19
                              , ka 28 21
                              , ka 28 25
                              , ka 29 18
                              , ka 29 20
                              , ka 29 22
                              , ka 30 19
                              , ka 30 23
			      ]
             }

ham2 = symmetrisch
       Graph { tafel  = listToFM [ (i, kn i (show i)) | i<-[1..28] ]
             , kanten = mkSet [ ka 5 3
                              , ka 6 4
                              , ka 7 1
                              , ka 8 2
                              , ka 9 3
                              , ka 10 2
                              , ka 10 7
                              , ka 11 1
                              , ka 11 3
                              , ka 11 8
                              , ka 12 2
                              , ka 12 4
                              , ka 12 5
                              , ka 12 9
                              , ka 13 3
                              , ka 13 6
                              , ka 14 4
                              , ka 14 7
                              , ka 15 6
                              , ka 15 12
                              , ka 16 5
                              , ka 16 7
                              , ka 16 13
                              , ka 17 6
                              , ka 17 8
                              , ka 17 10
                              , ka 17 14
                              , ka 18 7
                              , ka 18 9
                              , ka 18 11
                              , ka 19 8
                              , ka 19 12
                              , ka 20 11
                              , ka 20 17
                              , ka 21 10
                              , ka 21 12
                              , ka 21 18
                              , ka 22 11
                              , ka 22 13
                              , ka 22 15
                              , ka 22 19
                              , ka 23 12
                              , ka 23 14
                              , ka 23 16
                              , ka 24 13
                              , ka 24 17
                              , ka 25 16
                              , ka 25 22
                              , ka 26 15
                              , ka 26 17
                              , ka 26 23
                              , ka 27 16
                              , ka 27 18
                              , ka 27 20
                              , ka 27 24
                              , ka 28 17
                              , ka 28 19
                              , ka 28 21
                              ]
             }
