-- | Graph Datenstruktur mit elementaren Klasseninstanzen
--
-- autor Georg Martius
-- mai99dgf@studserv.uni-leipzig.de


module Graph.Graph ( 
      module Set
    , module ReadSet
    ,  Graph (knoten, kanten),Kante (von, nach), kante
    )
where

import Set
import ReadSet

-------------------------------------------------------------------------------

data Graph a  = Graph
	      { knoten    :: Set a
	      , kanten    :: Set (Kante a)
	      } deriving (Read)


data Kante a  = Kante
	      { von       :: a
	      , nach      :: a
        } deriving (Eq, Ord, Read)

kante :: a -> a -> (Kante a)
kante x y = Kante { von = x, nach = y }
