-- | zum visualisieren eines Graphen mittels graphiz-tools

--  - gerichtet graphen: dot -Tformat -o output.format 
--  - unrichtet graphen: neato -Tformat -o output.format 
--
-- autor Georg Martius
-- mai99dgf@studserv.uni-leipzig.de

module Graph.Vis 
    (showDirGraph
    ,showUndirGraph
    )
    where

import Graph.Type

import Set

-------------------------------------------------------------------------------
-- Output for Graphviz-Tool dot and neato

-- for a.out | dot -Tps -o graph.ps
showDirGraph::(Show a, Ord a) => Graph a -> String
showDirGraph g = unlines 
		  $ [ "digraph G { "] 
--      ++[ showKnoten k i | (k,i) <- zip (setToList (knoten g)) [1..] ] 
      ++[ showDirKante k | k <- (setToList (kanten g)) ] 
      ++["}"]

-- for a.out | neato -Tps -o graph.ps
showUndirGraph::(Show a, Ord a) => Graph a -> String
showUndirGraph g = unlines 
		  $ [ "graph G { " ] 
--      ++[ showKnoten k i | (k,i) <- zip (setToList (knoten g)) [1..] ] 
      ++[ showUndirKante k | k <- (setToList (kanten g)) ] 
      ++["}"]


showKnoten::(Show a, Ord a)=> a -> Int -> String
showKnoten k i = " " ++ show i ++ " [label = \"" ++ show k ++ "\" ];" -- insert color=blue ... 

showDirKante::(Show a, Ord a)=> Kante a -> String
showDirKante k = " " ++ show(von k) ++ " -> " ++ show(nach k) ++ ";" 

showUndirKante::(Show a, Ord a)=> Kante a -> String
showUndirKante k = " " ++ show(von k) ++ " -- " ++ show(nach k) ++ ";" 

-- test stuff
--main= putStr $ showUndirGraph graph_vis1
--main = show graph_vis1
graph_vis1 = Graph { knoten  = mkSet [ i | i<-[1..12] ]
                   , kanten  = mkSet [ kante 1 2,
                                      kante 1 3,
                                      kante 1 4,
                                      kante 1 5,
                                      kante 1 6,
                                      kante 1 7,
                                      kante 1 8,
                                      kante 1 9,
                                      kante 1 10,
                                      kante 1 11,
                                      kante 1 12,
                                      kante 2 3,
                                      kante 3 4,
                                      kante 4 5,
                                      kante 5 6,
                                      kante 6 7,
                                      kante 7 8,                                
                                      kante 8 9,
                                      kante 9 10,
                                      kante 10 11,
                                      kante 11 12,
                                      kante 12 2
                                     ]
                }

