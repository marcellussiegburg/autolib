module Graph.Util where

import Graph.Graph
import Set
import Monad ( guard ) -- old style
import Graph.Beispiele
import FiniteMap
import Sort

anzKnoten :: Graph a -> Int
anzKnoten g = cardinality $ knoten g

anzKanten :: Graph a -> Int
anzKanten g = cardinality $ kanten g

--Programmiert von Markus Kreuz--
existKante :: Ord a => Graph a -> a -> a -> Bool
existKante g x y = elementOf Kante{von=x,nach=y} (kanten g)


------------------------------------------------------------------------------------------
-- autor M.Lindemeyer
--
-- | ueberprueft, ob Knotenmenge leer
isKnotenEmptyset :: Eq a => Graph a -> Bool
isKnotenEmptyset a = (anzKnoten a == 0)

-- | ueberprueft, ob Kantenmengeleer
isKanteEmptyset :: Eq a => Graph a -> Bool
isKanteEmptyset a = (anzKanten a == 0)

-- | ueberprueft, ob Knotenanzahl zweier Graphen identisch
knotensetId :: (Eq a, Eq b) => Graph a -> Graph b -> Bool
knotensetId a b = (anzKnoten a) == (anzKnoten b)

-- | ueberprueft, ob Kantenanzahl zweier Graphen identisch 
kantensetId :: (Eq a, Eq b) => Graph a -> Graph b -> Bool
kantensetId a b = (anzKanten a) == (anzKanten b)


--------------------------------------------------------------------------------
-- Begin Alexander Kiel


-- | ueberprueft, ob es alle Knoten gibt, auf die die Kanten zeigen
-- gibt den ersten Knoten zurück, den es nicht in der Knotenmenge gibt
kantenPassenZuKnoten :: (Ord a, Eq a) => Graph a -> (Bool, Maybe a)
kantenPassenZuKnoten graph =
	xxand $ map (gibtEsKnotenDerKante graph) (kantenliste graph)

-- prüft, ob die beiden Knoten der Kante im Graph exsistieren
-- gibt zusätlich vieleicht den ersten nicht exsistierenden Knoten zurück
gibtEsKnotenDerKante :: Ord a => Graph a -> Kante a -> (Bool, Maybe a)
gibtEsKnotenDerKante graph kante =
	xand
	[ ((elementOf (von kante) (knoten graph)), von kante)
	, ((elementOf (nach kante) (knoten graph)), nach kante)
	]

-- macht eine logische AND Verknüpfung über eine Liste aus Tupeln
-- ein Tupel besteht aus dem Boolschen Wert und einem beliebigen Datum,
-- welches anzeigen soll, was hier falsch oder richtig ist
-- zurückgegeben wird entweder (True, Nothing), wenn nix falsch war
-- oder (False, Just a) wobei a das erste falsche Datum ist
xand :: [(Bool, a)] -> (Bool, Maybe a)
xand [] = (True, Nothing)
xand ((False, a):_) = (False, Just a)
xand ((True, _):rest) = xand rest

-- das gleiche wie xand, nur, dass die Eingabe schon (Bool, Maybe a) ist
xxand :: [(Bool, Maybe a)] -> (Bool, Maybe a)
xxand [] = (True, Nothing)
xxand ((False, Nothing):_) = (False, Nothing)
xxand ((False, Just a):_) = (False, Just a)
xxand ((True, _):rest) = xxand rest              

loescheDoppelte :: Ord a => [a] -> [a]
loescheDoppelte [] = []
-- falsch (Alex) loescheDoppelte (a:b:c) = if (a==b)
--							then [] ++ loescheDoppelte (b:c)
--                            else [a] ++ loescheDoppelte (b:c)
loescheDoppelte a = setToList $ mkSet a 
loescheDoppelte a = a


-- Ende Alexnder Kiel
--------------------------------------------------------------------------------


------------------------------------------------------------------------------------------
-- autor M.Lindemeyer
--
-- Kanten, Knoten in eine Liste 
kantenliste :: Graph a -> [Kante a]
kantenliste a = setToList(kanten a)
knotenliste :: Graph a -> [a]
knotenliste a = setToList(knoten a)

------------------------------------------------------------------------------------------
-- autor M.Lindemeyer
--
-- | Nachfolger eines Knotens aus einem Graph ermitteln
nachfolger :: Eq a => Graph a -> a -> [a]
nachfolger a b = do 
                x <- (kantenliste a)
                guard $ or [(nach x) == b, (von x) == b]
                if (nach x) == b
                  then return (von x)
                  else return (nach x)

------------------------------------------------------------------------------------------
-- autor M.Lindemeyer
--
-- | gradzahl zu einem knoten
grad :: (Ord a, Eq a) => Graph a -> a -> Int
grad a b = length(nachfolger a b)

------------------------------------------------------------------------------------------
-- autor M.Lindemeyer
--
-- | liste der gradzahlen aller nachbarn zu einem knoten
grad2 :: (Ord a, Eq a) => Graph a -> a -> [Int]
grad2 a b = do
            x <- nachfolger a b
            return (grad a x)

------------------------------------------------------------------------------------------
-- autor M.Lindemeyer
--
-- | zusammenhaeng des graphen feststellen

isZusammen :: (Eq a, Ord a) => Graph a -> Bool
isZusammen a = isEineMap (zusammen (kantenliste a) emptyFM) (knotenliste a) 

zusammen :: (Eq a, Ord a) => [Kante a] -> FiniteMap a a -> FiniteMap a a
zusammen [] fm = fm
zusammen (a:ax) fm = let  
						x = (root (von a) fm)
						y = (root (nach a) fm) 
                     in
                     	if (x==y)
						 then zusammen ax fm
						 else zusammen ax (addToFM fm x y)
                      
isEineMap :: Ord a => FiniteMap a a -> [a] -> Bool
isEineMap fm (x:y:z) = if ((root x fm) == (root y fm))
						then isEineMap fm (y:z)
                        else False
isEineMap fm x = True

root :: Ord a => a -> FiniteMap a a -> a
root knoten fm = if elemFM knoten fm
								then root (maybe knoten (id) (lookupFM fm knoten)) fm
								else knoten

finite = addToFM emptyFM 'a' 'b'
finite2 = addToFM finite 'b' 'c'



-------------------------------------------------------------------------------

{-mapG :: (Ord a, Ord b) 
     => (String -> String) -> (a -> b) -> (Graph a -> Graph b)
mapG fi f g = Graph 
     { knoten = setToFM [ ( f i, v { idy = f (idy v), info = fi (info v) } )
			| k <- setToList knoten
			]
     , kanten = mapSet ( \ k -> ka (f (von k)) (f (nach k)) ) ( kanten g )
     }

abzähl :: Ord a => Graph a -> Graph Integer
abzähl g = 
    let fm  = listToFM $ zip ( knotenl g ) [ 1 .. ]
        f   = lookupWithDefaultFM fm (error "abzähl") 
    in  mapG id f g

gefährliche_summe :: Ord a => Graph a -> Graph a -> Graph a
gefährliche_summe g1 g2 = 
    Graph { tafel = plusFM (tafel g1) (tafel g2)
	  , kanten = kanten g1 `union` kanten g2
	  }

summe :: (Ord a, Ord b) => Graph a -> Graph b -> Graph (Either a b)
summe g1 g2 =
    mapG ('L' :) Left g1 `gefährliche_summe` mapG ('R' :) Right g2

-----------------------------------------------------------------------------

mapK :: Ord a
     => (Set (Kante a) -> Set (Kante a)) -> (Graph a -> Graph a)
mapK f g = Graph { tafel = tafel g, kanten = f ( kanten g ) }

verbinde :: Ord a => Graph a -> a -> a -> Graph a
verbinde g x y = mapK (`union` mkSet [ ka x y ]) g

verbinde_symmetrisch :: Ord a => Graph a -> a -> a -> Graph a
verbinde_symmetrisch g x y = verbinde (verbinde g x y) y x

entferneKante :: Ord a => Graph a -> a -> a -> Graph a
entferneKante g x y = mapK (`minusSet` mkSet [ ka x y ]) g

entferneKante_symmetrisch :: Ord a => Graph a -> a -> a -> Graph a
entferneKante_symmetrisch g x y = entferneKante (entferneKante g x y) y x

rückwärts :: Ord a => Graph a -> Graph a
rückwärts = mapK (mapSet ( \ k -> ka (nach k) (von k) ))

symmetrisch :: Ord a => Graph a -> Graph a
symmetrisch g = mapK (`union` kanten (rückwärts g)) g

reflexiv :: Ord a => Graph a -> Graph a
reflexiv g = mapK (`union` mkSet [ ka x x | x<-(knotenl g) ]) g

unabhängig_auf :: Ord a => [ a ] -> Graph a
unabhängig_auf xs = 
    Graph { kanten = emptySet
	  , tafel  = listToFM [ (k, kn k "") | k <- xs ]
	  }

-- ohne Schlinge
komplement :: Ord a => Graph a -> Graph a
komplement g = 
   Graph { tafel  = tafel g 
	 , kanten = mkSet [ ka k k' 
                          | k <- (knotenl g) 
                          , k' <- (knotenl g)
                          , k < k' || k' < k
                          ] `minusSet` (kanten g)
	 }

clique_auf :: Ord a => [ a ] -> Graph a
clique_auf = komplement . unabhängig_auf

voll_auf :: Ord a => [ a ] -> Graph a
voll_auf = reflexiv . clique_auf

runde_auf :: Ord a => [ a ] -> [(a,a)]
runde_auf xs = zip xs (tail xs ++ [head xs])

kreis_auf :: Ord a => [ a ] -> Graph a
kreis_auf xs =
    foldr ( \ (x,y) g -> verbinde g x y) 
          (unabhängig_auf xs) (runde_auf xs)

istWeg :: Ord a => Graph a -> [ a ] -> Bool
istWeg g xs = isEmptySet (mkSet (map ( \ (x,y) -> ka x y ) (zip xs (tail xs)))
                         `minusSet` kanten g)

-------------------------------------------------------------------------------

vorgänger :: Eq a => (Graph a) -> a -> [ a ]
vorgänger g x = do
	  k @ Kante { } <- setToList $ kanten g
	  guard $ nach k == x
	  return $ von k

nachfolger :: Eq a => (Graph a) -> a -> [ a ]
nachfolger g x = do
	  k @ Kante { } <- setToList $ kanten g
	  guard $ von k == x
	  return $ nach k

ein_grad, aus_grad, grad :: Eq a => (Graph a) -> a -> Int
ein_grad g k = length $ vorgänger g k
aus_grad g k = length $ nachfolger g k
grad g k = ein_grad g k + aus_grad g k

neuerKnoten :: Ord a => Graph a -> a -> String -> Graph a
neuerKnoten g a i =
  Graph { tafel   = addToFM (tafel g) a (kn a i)
         , kanten = kanten g
         }

fehlendeKnoten :: Ord a => Graph a -> [a] -> Set a
fehlendeKnoten g xs = knoten g `minusSet` mkSet xs

mehrfacheKnoten :: Ord a => [a] -> Set a
mehrfacheKnoten xs = 
  mkSet [ x 
        | (x,y)<-(fmToList (addListToFM_C (+) emptyFM [ (x,1) | x<-xs ]))
        , y>1 ]
-}
-------------------------------------------------------------------------------

--entferneKnoten :: Ord a => Graph a -> a -> Graph a
--entferneKnoten g k = teil g (setToList ((knoten g) `minusSet` mkSet [ k ]))

--teil :: Ord a => Graph a -> [ a ] -> Graph a
--teil g xs = let m = mkSet xs
--                erlaubt x = x `elementOf` m
--            in Graph { tafel  = filterFM ( \ x v -> erlaubt x ) (tafel g)
--                     , kanten = filterSet ( \ k -> erlaubt (von k) && erlaubt (nach k)) (kanten g) 
--                     }
