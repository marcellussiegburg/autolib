module Graph.Color where

import Graph
import Graph.Util

import Data.Set
import Data.FiniteMap

import Auswertung
import Right

-------------------------------------------------------------------------------

istFaerbung :: Ord a => Graph a -> [[a]] -> Bool
istFaerbung g xs = mkSet (gleichfarbKanten g xs) == mkSet [ mkSet [] ]
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
                                  ++ show k ++ "-F�rbung."

   let alleKn = [ x | l<-student, x<-(setToList(mkSet l)) ]

   let eKn    = setToList (mkSet alleKn `minusSet` 
                           mkSet [ x | x<-(knotenl graph) ])
   let exctKn = case eKn of
                [] -> Right $ "Alle gef�rbten Knoten geh�ren zum Graph."
                _  -> Left  $ "Folgende gef�rbte Knoten geh�ren nicht zum Graph:\n"
		              ++ show eKn

   let fKn    = setToList (fehlendeKnoten graph alleKn)
   let fehlKn = case fKn of
                [] -> Right $ "Alle Knoten sind gef�rbt."
                _  -> Left  $ "Folgende Knoten sind nicht gef�rbt:\n"
		              ++ show fKn

   let mKn    = setToList (mehrfacheKnoten alleKn)
   let mehrKn = case mKn of
                [] -> Right $ "Keine Knoten sind mit mehr als einer Farbe gef�rbt."
                _  -> Left  $ "Folgende Knoten sind mit mehr als einer Farbe gef�rbt:\n"
                              ++ show mKn

   let istF   = case istFaerbung graph student of
                True  -> Right $ "Keine Knoten gleicher Farbe sind verbunden." 
                                 ++ "Das ist wirklich eine "
                                 ++ show k ++ "-F�rbung."
                False -> Left  $ unlines
                      $  [ "Folgende Knoten gleicher Farbe sind verbunden:" ]
                         ++ nice (gleichfarbKanten graph student)

   putStrLn $ unlines 
     $ [ "Ich bin der automatische Korrektor f�r Aufgabe " ++ msg
       , ""
       , "Gesucht ist eine " ++ show k ++ "-F�rbung eines Graphen."
       , "" -- , show graph
       , "Ihre F�rbung lautet:"
       ] ++ nice student

   muss fCount $ muss fehlKn $ muss mehrKn $ muss exctKn $ muss istF $ right

nice :: Show a => [ a ] -> [ String ]
nice xs = [ "Farbe " ++ show n ++ ": " ++ show f
          | (n,f)<-(zip [1..] [ f | f <- xs ]) 
          ]
