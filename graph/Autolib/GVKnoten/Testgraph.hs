module Testgraph where

import Graph.Graph

test_graph_int :: Graph Int
test_graph_int = Graph (mkSet[0, 1, 2])	
                       (mkSet[Kante 0 1, Kante 0 2]) 

test_graph_str :: Graph String
test_graph_str = Graph (mkSet["0", "1", "2"])
                       (mkSet[Kante "0" "1", Kante "0" "2", Kante "0" "3"
						 	        ,Kante "2" "4", Kante "2" "5", Kante "3" "6"]) 

test_graph_stt :: Graph String
test_graph_stt = Graph (mkSet["V0", "V1", "V2"])
                       (mkSet[Kante "V0" "V1", Kante "V0" "V2", Kante "V0" "V3"
						 	        ,Kante "V2" "V4", Kante "V2" "V5", Kante "V3" "V6"]) 

test_graph_tre :: Graph String
test_graph_tre = Graph ( mkSet [ "011\nV"
                               , "101\nG", "012\nG", "021\nG"
                               , "110\nV", "102\nG", "201\nG", "022\nV"
                               , "120\nG", "210\nG", "202\nV"
                               , "220\nV" ] )
                       ( mkSet [ Kante "011\nV" "101\nG"
                               , Kante "011\nV" "012\nG"
                               , Kante "011\nV" "021\nG"
                               , Kante "101\nG" "110\nV"
                               , Kante "101\nG" "102\nG"
                               , Kante "101\nG" "201\nG"
                               , Kante "012\nG" "102\nG"
                               , Kante "012\nG" "022\nV"
                               , Kante "021\nG" "022\nV"
                               , Kante "110\nV" "120\nG"
                               , Kante "110\nV" "210\nG"
                               , Kante "102\nG" "202\nV"
                               , Kante "201\nG" "210\nG"
                               , Kante "201\nG" "202\nV"
                               , Kante "120\nG" "220\nV"
                               , Kante "210\nG" "220\nV" ] )

test_graph_tree :: Graph String
test_graph_tree = Graph ( mkSet [ "011999"
                                , "101999", "012999", "021999"
                                , "110999", "102999", "201999", "022999"
                                , "120999", "210999", "202999"
                                , "220999" ] )
                        ( mkSet [ Kante "011999" "101999"
                                , Kante "011999" "012999"
                                , Kante "011999" "021999"
                                , Kante "101999" "110999"
                                , Kante "101999" "102999"
                                , Kante "101999" "201999"
                                , Kante "012999" "102999"
                                , Kante "012999" "022999"
                                , Kante "021999" "022999"
                                , Kante "110999" "120999"
                                , Kante "110999" "210999"
                                , Kante "102999" "202999"
                                , Kante "201999" "210999"
                                , Kante "201999" "202999"
                                , Kante "120999" "220999"
                                , Kante "210999" "220999" ] )
						 			  
