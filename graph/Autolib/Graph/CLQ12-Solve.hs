module Main where

import Graph
import Graph_Util
import Graph_Clique_Solve

import Set
import FiniteMap

main = do 
     putStrLn $ unlines 
              $ map ( \g -> show (alleGrösstenCliquen g [[]]) )
                    [clq1,clq2]


clq1 = Graph { tafel = listToFM [ (i, kn i (show i)) | i<-[1..23] ]
             , kanten = mkSet [ ka 1 2
                              , ka 1 3
                              , ka 1 5
                              , ka 1 7
                              , ka 1 9
                              , ka 1 10
                              , ka 2 3
                              , ka 2 4
                              , ka 2 5
                              , ka 2 6
                              , ka 2 8
                              , ka 2 10
                              , ka 3 6
                              , ka 4 5
                              , ka 4 6
                              , ka 4 7
                              , ka 4 8
                              , ka 5 8
                              , ka 5 9
                              , ka 6 9
                              , ka 7 9
                              , ka 7 10
                              , ka 8 9
			      ]
             }

clq2 = Graph { tafel = listToFM [ (i, kn i (show i)) | i<-[1..35] ]
             , kanten = mkSet [ ka 1 8
                              , ka 1 11
                              , ka 1 12
                              , ka 1 15
                              , ka 2 3
                              , ka 2 6
                              , ka 2 7
                              , ka 2 9
                              , ka 2 10
                              , ka 2 13
                              , ka 3 7
                              , ka 3 14
                              , ka 4 8
                              , ka 4 9
                              , ka 4 11
                              , ka 4 12
                              , ka 4 13
                              , ka 5 6
                              , ka 5 11
                              , ka 5 14
                              , ka 5 15
                              , ka 6 7
                              , ka 6 8
                              , ka 6 10
                              , ka 7 11
                              , ka 7 13
                              , ka 7 15
                              , ka 8 11
                              , ka 8 15
                              , ka 9 12
                              , ka 9 13
                              , ka 10 12
                              , ka 10 14
                              , ka 11 13
                              , ka 11 15
			      ]
             }
