module  Graph.Beispiele where


import Graph.Graph
import Data.Set


---------------------------------------------
--viele viele beispiele

kanten11 = [Kante {von = 'a', nach = 'b'},Kante {von = 'a', nach = 'd'},
  Kante {von = 'a', nach = 'g'},Kante {von = 'b', nach = 'c'},
  Kante {von = 'b', nach = 'd'},Kante {von = 'c', nach = 'e'},
  Kante {von = 'd', nach = 'e'},Kante {von = 'd', nach = 'f'},
  Kante {von = 'g', nach = 'f'}]

kanten12 = [Kante {von = 'a', nach = 'b'},Kante {von = 'b', nach = 'c'},Kante {von = 'a', nach = 'c'}]


bspgraph10 = Graph{knoten=mkSet['a','b','c','d','e','f','g'], 
    kanten=mkSet[
      kante 'a' 'b',
      kante 'a' 'c',
      kante 'a' 'd',
      kante 'b' 'd',
      kante 'b' 'c',
      kante 'c' 'e',
      kante 'd' 'g',
      kante 'e' 'f',                                        
      kante 'g' 'f',
      kante 'd' 'c',                                        
      kante 'd' 'e'
      ]}
bspgraph101 = Graph{knoten=mkSet['e','f','g','h'], 
    kanten=mkSet[
      kante 'e' 'f',
      kante 'f' 'g',                                        
      kante 'g' 'h'
      ]}
bspgraph102 = Graph{knoten=mkSet['e','f','g','h'], 
    kanten=mkSet[
      kante 'e' 'f',
      kante 'f' 'g',                                        
      kante 'g' 'h',
      kante 'h' 'e'
      ]}
bspgraph103 = Graph{knoten=mkSet['e','f','g','h','i','j'], 
    kanten=mkSet[
      kante 'e' 'f',
      kante 'f' 'g',                                        
      kante 'g' 'h',
      kante 'i' 'j'
      ]}
bspgraph11 = Graph{knoten=mkSet['a','b','c','d','e','i','g'], 
    kanten=mkSet[
      kante 'a' 'b',
      kante 'a' 'd',
      kante 'a' 'g',
      kante 'b' 'd',
      kante 'b' 'c',
      kante 'd' 'e',
      kante 'd' 'i',
      kante 'g' 'i',
      kante 'c' 'e',
      kante 'd' 'g',                                        
      kante 'g' 'e'                                            
      ]}

bspgraph1 = Graph{knoten=mkSet['a','b','c','d','e','f'], 
    kanten=mkSet[
      kante 'a' 'b',
      kante 'a' 'd',
      kante 'b' 'd',
      kante 'b' 'e',
      kante 'b' 'c',
      kante 'd' 'e',
      kante 'd' 'f',
      kante 'e' 'f'                                        
      ]}
bspgraph2 = Graph{knoten=mkSet['a','b','c','d','e','f'], 
    kanten=mkSet[
      kante 'a' 'b',
      kante 'a' 'd',
      kante 'b' 'd',
      kante 'b' 'e',
      kante 'b' 'c',
      kante 'd' 'e',
      kante 'd' 'f',
      kante 'e' 'f'                                        
      ]}
bspgraph3 = Graph{knoten=mkSet['a','b','c','d','e','f'], 
    kanten=mkSet[
      kante 'a' 'b',
      kante 'b' 'f',
      kante 'b' 'e',
      kante 'b' 'd',
      kante 'c' 'd',
      kante 'c' 'e',
      kante 'd' 'e',
      kante 'd' 'f'                                        
      ]}
bspgraph4 = Graph{knoten=mkSet['a','b','c','d','e','f'], 
    kanten=mkSet[
      kante 'a' 'b',
      kante 'b' 'f',
      kante 'b' 'e',
      kante 'b' 'd',
      kante 'c' 'd',
      kante 'c' 'e',
      kante 'd' 'e',
      kante 'd' 'a'                                        
      ]}
bspgraph5 = Graph{knoten=mkSet['a','b','c','d','e','g'], 
    kanten=mkSet[
      kante 'b' 'a',
      kante 'b' 'g',
      kante 'b' 'e',
      kante 'b' 'd',
      kante 'c' 'd',
      kante 'c' 'e',
      kante 'd' 'e',
      kante 'd' 'a'                                        
      ]}
bspgraph6 = Graph{knoten=mkSet['a','b','c','d','e','g','h'], 
    kanten=mkSet[
      kante 'b' 'a',
      kante 'b' 'g',
      kante 'b' 'e',
      kante 'b' 'd',
      kante 'c' 'd',
      kante 'c' 'e',
      kante 'd' 'e',
      kante 'h' 'h',
      kante 'd' 'a'                                        
      ]}
bspgraph7 = Graph{knoten=mkSet[], 
    kanten=mkSet[
      kante 'b' 'a',
      kante 'b' 'g',
      kante 'b' 'e',
      kante 'b' 'd',
      kante 'c' 'd',
      kante 'c' 'e',
      kante 'd' 'e',
      kante 'h' 'h',
      kante 'd' 'a'                                        
      ]}

