-- | Graph Datenstruktur mit elementaren Klasseninstanzen

-- autor Georg Martius
-- mai99dgf@studserv.uni-leipzig.de

-- geänderte ToDoc/Show-Instanzen joe@informatik.uni-leipzig.de

module Autolib.Graph.Type (
      module Autolib.Graph.Graph
    , module Autolib.Graph.Layout
--    , module ToDoc
    , module Autolib.Iso
    , module Autolib.Number
    , display
    , layout
    )
 where
 
-- (Graph (knoten, kanten),Kante (von, nach), kante)

import Autolib.Graph.Graph
import Autolib.Graph.Isomorph
import Autolib.Graph.Display
import Autolib.Graph.Layout

import Autolib.GVKnoten.Layout

import Autolib.ToDoc
import Autolib.FiniteMap
import Autolib.Iso
import Autolib.Number

-------------------------------------------------------------------------------


-- | Instanziering von der Isomorphieklasse fuer Graphen 
instance (GraphC a) => Iso (Graph a) 
	where
	    iso g1 g2 = isIsomorph g1 g2

-- | Instanziering von der Number
instance ( GraphC a, GraphC Int ) => Number (Graph a) (Graph Int) 
	where
	    number g = graphToGraphInt g

        

graphToGraphInt :: ( GraphC Int, GraphC knotenTyp ) 
		=> Graph knotenTyp -> Graph Int
graphToGraphInt (Graph { knoten = knoten, kanten =  kanten }) =
    let
        knotenToInt = mkKnotenToInt emptyFM (setToList knoten) 0
        knotenSet = eltSet knotenToInt
        kantenSet = smap (kantenMapping knotenToInt) kanten
    in
        mkGraph knotenSet kantenSet 
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
 
  
