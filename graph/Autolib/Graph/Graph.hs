-- | Graph Datenstruktur mit elementaren Klasseninstanzen

-- autor Georg Martius
-- mai99dgf@studserv.uni-leipzig.de

-- -- $Id$


module Autolib.Graph.Graph ( 
      module  Autolib.Set
      , module Autolib.Informed
      , GraphC 
     ,  Graph (..), mkGraph
    , Kante (..), kante
) where

import Autolib.Graph.Kante
import qualified Autolib.Graph.Reading.Type as R

import Autolib.Set
import Autolib.Informed
import Autolib.ToDoc
import Autolib.Reader

import Autolib.Boxing.Position
import Autolib.FiniteMap

import Autolib.Xml
import Text.XML.HaXml.Haskell2Xml
import Data.Typeable

-------------------------------------------------------------------------------

data Graph a  = Graph
	      { knoten    :: Set a
	      , kanten    :: Set (Kante a)
	      -- neue komponenten (nicht show/read-fähig)
	      , graph_info :: Doc
	      , graph_texinfo :: String
	      -- partiell (entspricht pinning in neato)
	      , graph_layout  :: FiniteMap a Position
	      , bounding :: Position
	      , layout_hints :: [ String ]
	      , layout_program :: String
	      , show_labels :: Bool
	      } 
    deriving Typeable

instance Informed ( Graph a) where
    info = graph_info
    informed i g = g { graph_info = i }
    texinfo = graph_texinfo
    texinformed i g = g { graph_texinfo = i }

mkGraph :: Set a -> Set (Kante a) -> Graph a
mkGraph v e = Graph
	    { knoten = v
	    , kanten = e
	    , graph_info = text "mkGraph"
	    , graph_texinfo = "G"
	    , graph_layout = emptyFM
	    , bounding = 0
	    , layout_hints = [ "-s" ]
	    , layout_program = "neato"
	    , show_labels = True
	    }

class (  Haskell2Xml a, Haskell2Xml (Kante a)
      , Ord a, ToDoc a, ToDoc [a], Reader a, Reader [a]
      ) => GraphC a 


instance (  Haskell2Xml a, Haskell2Xml (Kante a)
      , Ord a, ToDoc a, ToDoc [a], Reader a, Reader [a]
      ) => GraphC a 


instance GraphC a 
    => Container ( Graph a ) ( R.Graph a ) where
    label _ = "Graph"
    pack g =  R.Graph { R.knoten = knoten g 
				, R.kanten = kanten g
   				}
    unpack h = mkGraph ( R.knoten h ) (R.kanten h )

instance GraphC a => Reader ( Graph a ) where
    readerPrec d = do
        h <- readerPrec d 
        return $ unpack h

instance GraphC a => ToDoc ( Graph a ) where
        toDocPrec p g = toDocPrec p 
		      $ pack g





