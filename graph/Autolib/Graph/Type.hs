-- | Graph Datenstruktur mit elementaren Klasseninstanzen
--
-- autor Georg Martius
-- mai99dgf@studserv.uni-leipzig.de

-- geänderte ToDoc/Show-Instanzen joe@informatik.uni-leipzig.de

module Graph.Type (
      module Graph.Graph
    , module ToDoc
    , module Iso
    , module Number
    , display
    , layout
    )
 where
 
-- (Graph (knoten, kanten),Kante (von, nach), kante)

import Graph.Graph
import Graph.Isomorph
import Graph.Display
import GVKnoten.Layout

import ToDoc
import FiniteMap
import Iso
import Number

-------------------------------------------------------------------------------

instance Eq a => Eq  (Graph a)
               where x == y = (knoten x == knoten y) && (kanten x == kanten y)   

-- | Instanziering von der Isomorphieklasse fuer Graphen 
instance (Eq a, Ord a) => Iso (Graph a) 
	where
	    iso g1 g2 = isIsomorph g1 g2

-- | Instanziering von der Number
instance (Ord a) => Number (Graph a) (Graph Int) 
	where
	    number g = graphToGraphInt g

        

graphToGraphInt :: Ord knotenTyp => Graph knotenTyp -> Graph Int
graphToGraphInt (Graph knoten kanten) =
    let
        knotenToInt = mkKnotenToInt emptyFM (setToList knoten) 0
        knotenSet = eltSet knotenToInt
        kantenSet = mapSet (kantenMapping knotenToInt) kanten
    in
        Graph knotenSet kantenSet 
    where
        mkKnotenToInt :: Ord knotenTyp
            => FiniteMap knotenTyp Int -> [knotenTyp] -> Int ->
            FiniteMap knotenTyp Int
        mkKnotenToInt fmap (knoten:rest) x =
            mkKnotenToInt (addToFM fmap knoten x) rest (x+1)
        mkKnotenToInt fmap [] x = fmap
        kantenMapping :: Ord knotenTyp
            => FiniteMap knotenTyp Int -> Kante knotenTyp -> Kante Int
        kantenMapping fmap kante =
            Kante
                { von = maybe (-1) id (lookupFM fmap (von kante))
                , nach = maybe (-1) id (lookupFM fmap (nach kante))
                }
        keySet :: Ord keyType => FiniteMap keyType eltType -> Set keyType
        keySet fmap = mkSet $ keysFM fmap
        eltSet :: Ord eltType => FiniteMap keyType eltType -> Set eltType
        eltSet fmap = mkSet $ eltsFM fmap
-------------------------------------------------------------------------------
 
  
