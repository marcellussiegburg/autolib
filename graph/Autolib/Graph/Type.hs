-- | Graph Datenstruktur mit elementaren Klasseninstanzen
--
-- autor Georg Martius
-- mai99dgf@studserv.uni-leipzig.de

-- geänderte ToDoc/Show-Instanzen joe@informatik.uni-leipzig.de


module Graph.Type (
      module Graph.Graph
    , module ToDoc
    , module Iso
    )
 where
 
-- (Graph (knoten, kanten),Kante (von, nach), kante)

import Graph.Graph
import Graph.Isomorph
import ToDoc
import Iso

-------------------------------------------------------------------------------

instance Eq a => Eq  (Graph a)
               where x == y = (knoten x == knoten y) && (kanten x == kanten y)   

-- | Instanziering von der Isomorphieklasse fuer Graphen 
instance (Eq a, Ord a) => Iso (Graph a) 
	where
	    iso g1 g2 = isIsomorph g1 g2


-------------------------------------------------------------------------------
 
  
