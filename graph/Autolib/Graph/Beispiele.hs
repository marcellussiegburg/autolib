--  $Id$

module Autolib.Graph.Beispiele where


import Autolib.Graph.Graph
import Autolib.Graph.Basic
import Autolib.Graph.Ops


import Data.Set


bspgraph10 = 
  links ( independent $ mkSet['a','b','c','d','e','f','g'] )
    [
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
      ]

bspgraph101 = 
  links (independent $ mkSet['e','f','g','h'] )
    [
      kante 'e' 'f',
      kante 'f' 'g',                                        
      kante 'g' 'h'
      ]
bspgraph102 = links (independent $ mkSet['e','f','g','h'] ) 
    [
      kante 'e' 'f',
      kante 'f' 'g',                                        
      kante 'g' 'h',
      kante 'h' 'e'
      ]
bspgraph103 = links (independent $ mkSet['e','f','g','h','i','j'] ) 
    [
      kante 'e' 'f',
      kante 'f' 'g',                                        
      kante 'g' 'h',
      kante 'i' 'j'
      ]
bspgraph11 = links (independent $ mkSet['a','b','c','d','e','i','g'] ) 
    [
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
      ]

bspgraph1 = links (independent $ mkSet['a','b','c','d','e','f'] ) 
    [
      kante 'a' 'b',
      kante 'a' 'd',
      kante 'b' 'd',
      kante 'b' 'e',
      kante 'b' 'c',
      kante 'd' 'e',
      kante 'd' 'f',
      kante 'e' 'f'                                        
      ]
bspgraph2 = links (independent $ mkSet['a','b','c','d','e','f'] ) 
    [
      kante 'a' 'b',
      kante 'a' 'd',
      kante 'b' 'd',
      kante 'b' 'e',
      kante 'b' 'c',
      kante 'd' 'e',
      kante 'd' 'f',
      kante 'e' 'f'                                        
      ]
bspgraph3 = links (independent $ mkSet['a','b','c','d','e','f'] ) 
    [
      kante 'a' 'b',
      kante 'b' 'f',
      kante 'b' 'e',
      kante 'b' 'd',
      kante 'c' 'd',
      kante 'c' 'e',
      kante 'd' 'e',
      kante 'd' 'f'                                        
      ]
bspgraph4 = links (independent $ mkSet['a','b','c','d','e','f'] ) 
    [
      kante 'a' 'b',
      kante 'b' 'f',
      kante 'b' 'e',
      kante 'b' 'd',
      kante 'c' 'd',
      kante 'c' 'e',
      kante 'd' 'e',
      kante 'd' 'a'                                        
      ]
bspgraph5 = links (independent $ mkSet['a','b','c','d','e','g'] ) 
    [
      kante 'b' 'a',
      kante 'b' 'g',
      kante 'b' 'e',
      kante 'b' 'd',
      kante 'c' 'd',
      kante 'c' 'e',
      kante 'd' 'e',
      kante 'd' 'a'                                        
      ]
bspgraph6 = links (independent $ mkSet['a','b','c','d','e','g','h'] ) 
    [
      kante 'b' 'a',
      kante 'b' 'g',
      kante 'b' 'e',
      kante 'b' 'd',
      kante 'c' 'd',
      kante 'c' 'e',
      kante 'd' 'e',
      kante 'h' 'h',
      kante 'd' 'a'                                        
      ]
bspgraph7 = links (independent $ mkSet "abcdefgh" ) 
    [
      kante 'b' 'a',
      kante 'b' 'g',
      kante 'b' 'e',
      kante 'b' 'd',
      kante 'c' 'd',
      kante 'c' 'e',
      kante 'd' 'e',
      kante 'h' 'h',
      kante 'd' 'a'                                        
      ]


