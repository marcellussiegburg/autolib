module Graph_Color where

import Graph
import Graph_Util

import Set
import FiniteMap

import Auswertung
import Right

-------------------------------------------------------------------------------

istFärbung :: Ord a => Graph a -> [[a]] -> Bool
istFärbung g xs = mkSet (gleichfarbKanten g xs) == mkSet [ mkSet [] ]
                  &&
                  isEmptySet (mehrfacheKnoten xs)
                  &&
                  isEmptySet (fehlendeKnoten g 
			      (setToList (mkSet [ y | x <- xs, y <- x  ]))
                             )

gleichfarbKanten :: Ord a => Graph a -> [[a]] -> [ Set (Kante a) ]
gleichfarbKanten g = map ( \ l -> kanten ( teil g l ) )

-------------------------------------------------------------------------------

check graph k msg student = do
   let l      = length student
   let fCount = case l == k of
                True  -> Right $ "Das sind " ++ show l ++ " Farben."
                False -> Left  $ "Das sind " ++ show l ++ " Farben. "
                                  ++ "Gesucht ist aber eine " 
                                  ++ show k ++ "-Färbung."

   let alleKn = [ x | l<-student, x<-(setToList(mkSet l)) ]

   let eKn    = setToList (mkSet alleKn `minusSet` 
                           mkSet [ x | x<-(knotenl graph) ])
   let exctKn = case eKn of
                [] -> Right $ "Alle gefärbten Knoten gehören zum Graph."
                _  -> Left  $ "Folgende gefärbte Knoten gehören nicht zum Graph:\n"
		              ++ show eKn

   let fKn    = setToList (fehlendeKnoten graph alleKn)
   let fehlKn = case fKn of
                [] -> Right $ "Alle Knoten sind gefärbt."
                _  -> Left  $ "Folgende Knoten sind nicht gefärbt:\n"
		              ++ show fKn

   let mKn    = setToList (mehrfacheKnoten alleKn)
   let mehrKn = case mKn of
                [] -> Right $ "Keine Knoten sind mit mehr als einer Farbe gefärbt."
                _  -> Left  $ "Folgende Knoten sind mit mehr als einer Farbe gefärbt:\n"
                              ++ show mKn

   let istF   = case istFärbung graph student of
                True  -> Right $ "Keine Knoten gleicher Farbe sind verbunden." 
                                 ++ "Das ist wirklich eine "
                                 ++ show k ++ "-Färbung."
                False -> Left  $ unlines
                      $  [ "Folgende Knoten gleicher Farbe sind verbunden:" ]
                         ++ nice (gleichfarbKanten graph student)

   putStrLn $ unlines 
     $ [ "Ich bin der automatische Korrektor für Aufgabe " ++ msg
       , ""
       , "Gesucht ist eine " ++ show k ++ "-Färbung eines Graphen."
       , "" -- , show graph
       , "Ihre Färbung lautet:"
       ] ++ nice student

   muß fCount $ muß fehlKn $ muß mehrKn $ muß exctKn $ muß istF $ right

nice :: Show a => [ a ] -> [ String ]
nice xs = [ "Farbe " ++ show n ++ ": " ++ show f
          | (n,f)<-(zip [1..] [ f | f <- xs ]) 
          ]
