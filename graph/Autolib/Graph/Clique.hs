module Graph.Clique where

import Graph
import Graph.Util

import Data.Set
import Autolib.FiniteMap

import Auswertung
import Right 

-------------------------------------------------------------------------------

istClique :: Ord a => Graph a -> [a] -> Bool
istClique g x = symmetrisch (teil g x) == clique_auf x

-------------------------------------------------------------------------------

check graph k msg student = do
   let mKn    = setToList (mehrfacheKnoten student)
   let mehrKn = case mKn of
                [] -> Right $ "Keine Knoten sind mehrfach in der Clique enthalten."
                _  -> Left  $ "Folgende Knoten sind mehrfach in der Clique enthalten:\n"
                              ++ show mKn

   let eKn    = setToList (mkSet student `minusSet` 
                           mkSet [ x | x<-(knotenl graph) ])
   let exctKn = case eKn of
                [] -> Right $ "Alle Knoten geh�ren zum Graph."
                _  -> Left  $ "Folgende Knoten geh�ren nicht zum Graph:\n"
		              ++ show eKn

   let istC   = case istClique graph student of
                True  -> Right $ "Alle ben�tigten Kanten sind im Graph enthalten." 
                                 ++ " Das ist wirklich eine Clique."
                False -> Left  $ unlines
                      $  [ "Folgende Kanten sind nicht im Graph enthalten:\n"
                         , show (mkSet (map ( \ (x,y) -> ka x y ) (zip student (tail student))) `minusSet` (kanten graph))
                         ]

   let istGr  = case length student == k of
                True -> Right  $ "Das ist eine der gr�ssten Cliquen."
                False -> Left  $ "Das ist eine Clique der L�nge "
                                 ++ show (length student)
                                 ++ ". Es gibt jedoch noch gr�ssere Cliquen."

   putStrLn $ unlines 
     $ [ "Ich bin der automatische Korrektor f�r Aufgabe " ++ msg
       , ""
       , "Gesucht ist eine gr�sste Clique eines Graphen."
       , "" -- , show graph
       , "Ihre Clique lautet:"
       , show student
       ]

   muss mehrKn $ muss exctKn $ muss istC $ muss  istGr $ right
