module Graph.Hamilton where

import Graph
import Graph.Util

import Data.Set
import Data.FiniteMap

import Auswertung
import Right

-------------------------------------------------------------------------------

istHamilton :: Ord a => Graph a -> [a] -> Bool
istHamilton g (x:xs) = isEmptySet (fehlendeKnoten g (x:xs))
		       &&
		       isEmptySet (mehrfacheKnoten (x:xs))
		       && 
		       istWeg g ( [x] ++ xs ++ [x] )

-------------------------------------------------------------------------------

check graph msg student = do
   let fKn    = setToList (fehlendeKnoten graph student)
   let fehlKn = case fKn of
                [] -> Right $ "Alle Knoten sind im Graph enthalten."
                _  -> Left  $ "Folgende Knoten sind nicht im Graph enthalten:\n"
		              ++ show fKn

   let mKn    = setToList (mehrfacheKnoten student)
   let mehrKn = case mKn of
                [] -> Right $ "Keine Knoten sind mehrfach im Kreis enthalten."
                _  -> Left  $ "Folgende Knoten sind mehrfach im Kreis enthalten:\n"
                              ++ show mKn

   let eKn    = setToList (mkSet student `minusSet` 
                           mkSet [ x | x<-(knotenl graph) ])
   let exctKn = case eKn of
                [] -> Right $ "Alle Knoten gehören zum Graph."
                _  -> Left  $ "Folgende Knoten gehören nicht zum Graph:\n"
		              ++ show eKn

   let istH   = case istHamilton graph student of
                True  -> Right $ "Alle benötigten Kanten sind im Graph enthalten." 
                                 ++ "Das ist wirklich ein Hamiltonkreis."
                False -> Left  $ unlines
                      $  [ "Folgende Kanten sind nicht im Graph enthalten:\n"
                         , show (mkSet (map ( \ (x,y) -> ka x y ) (runde_auf student)) `minusSet` (kanten graph))
                         ]

   putStrLn $ unlines 
     $ [ "Ich bin der automatische Korrektor für Aufgabe " ++ msg
       , ""
       , "Gesucht war ein Hamiltonkreis eines Graphen."
       , "" -- , show graph
       , "Ihr Kreis lautet:"
       , show student
       ]

--   muß fehlKn $ muß mehrKn $ muß exctKn $ muß istH $ right
