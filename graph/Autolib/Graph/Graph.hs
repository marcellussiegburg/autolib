-- | Graph Datenstruktur mit elementaren Klasseninstanzen

-- autor Georg Martius
-- mai99dgf@studserv.uni-leipzig.de

-- -- $Id$


module Graph.Graph ( 
      module Sets
      , module Informed
    ,  Graph (..), mkGraph
    , Kante (..), kante
) where

import Graph.Kante
import qualified Graph.Reading.Type as R

import Sets
import Informed
import ToDoc
import Reader

import Boxing.Position
import FiniteMap

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

instance ( Ord a, Reader a, Reader [a], ToDoc [a], ToDoc a )
    => Reader ( Graph a ) where
    readerPrec d = do
        g <- readerPrec d 
        return $ mkGraph ( R.knoten g ) (R.kanten g )

instance ( Ord a, Reader a, Reader [a], ToDoc [a], ToDoc a )
    => ToDoc ( Graph a ) where
        toDocPrec p g = toDocPrec p 
		      $ R.Graph { R.knoten = knoten g 
				, R.kanten = kanten g
				}

instance  (Ord a,  Reader (Graph a) ) => Read ( Graph a ) where
     readsPrec = parsec_readsPrec
   
---------------------------------------------------------------------------

instance ToDoc (Graph a) => Show (Graph a) where
  show = render . toDoc



