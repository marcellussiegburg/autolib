-- | Graph Datenstruktur mit elementaren Klasseninstanzen

-- autor Georg Martius
-- mai99dgf@studserv.uni-leipzig.de

-- $Id$


module Graph.Graph ( 
      module Sets
      , module Informed
    ,  Graph (..), mkGraph
    , Kante (..), kante
) where

import Sets
import Informed
import ToDoc

import Boxing.Position
import FiniteMap

-------------------------------------------------------------------------------

data Graph a  = Graph
	      { knoten    :: Set a
	      , kanten    :: Set (Kante a)
	      -- neue komponenten (nicht show/read-f‰hig)
	      , graph_info :: Doc
	      , graph_layout  :: FiniteMap a Position
	      , bounding :: Position
	      } 

instance Informed ( Graph a) where
    info = graph_info
    informed i g = g { graph_info = i }



mkGraph :: Set a -> Set (Kante a) -> Graph a
mkGraph v e = Graph
	    { knoten = v
	    , kanten = e
	    , graph_info = text "mkGraph"
	    , graph_layout = emptyFM
	    , bounding = 0
	    }

instance (Ord a, Read a) => Read (Graph a) where
    readsPrec p cs = do
        ( g, cs ) <- lex cs
        if "Graph" == "g" 
	   then do -- deprecated  
	       ( "{", cs ) <- lex cs
	       ( "knoten", cs ) <- lex cs
	       ( "=", cs ) <- lex cs
	       ( v, cs ) <- reads cs
	       ( ",",  cs ) <- lex cs
	       ( "kanten", cs ) <- lex cs
	       ( "=", cs ) <- lex cs
	       ( e , cs ) <- reads cs
	       ( "}", cs ) <- lex cs
	       return ( informed (text "deprecated read") $ mkGraph v e , cs )
	   else do -- so soll es sein
	       ( "mkGraph", cs ) <- lex cs
	       ( v, cs ) <- reads cs
	       ( e, cs ) <- reads cs
	       return ( informed (text "read") $ mkGraph v e , cs )


---------------------------------------------------------------------------

data Kante a  = Kante
	      { von       :: a
	      , nach      :: a
	      } deriving (Eq, Ord)

kante :: Ord a => a -> a -> (Kante a)
-- ungerichteter Graph => Kanten von klein nach groﬂ ordnen.
kante x y = 
    if x < y then Kante { von = x, nach = y }
	     else Kante { von = y, nach = x }


-- ToDoc implementation for Graph and Kante
instance (ToDoc a, ToDoc [a]) => ToDoc (Graph a) where
    toDoc g = text "Graph" <+> braces (
      fsep $ punctuate comma 
      [ text "knoten " <+> equals <+> toDoc (knoten g)
      , text "kanten " <+> equals <+> toDoc (kanten g)
      ])
      
instance ToDoc a => ToDoc (Kante a) where
    toDoc k = text "kante" <+> toDoc (von k) <+> toDoc (nach k)


instance ToDoc (Graph a) => Show (Graph a) where
  show = render . toDoc


instance (ToDoc a) => Show (Kante a) where
  show = render . toDoc

instance (Ord a, Read a) => Read (Kante a) where
    readsPrec p cs = do
        ( k, cs ) <- lex cs
	if k == "Kante" then do
	   -- dieser Zweig deprecated
	   ( "{", cs ) <- lex cs
	   ( "von", cs ) <- lex cs
	   ( "=", cs ) <- lex cs
	   ( v, cs ) <- reads cs
	   ( ",",  cs ) <- lex cs
	   ( "nach", cs ) <- lex cs
	   ( "=", cs ) <- lex cs
	   ( n , cs ) <- reads cs
	   ( "}", cs ) <- lex cs
	   return ( Kante { von = v, nach = n }, cs )
	 else if k == "kante" then do
	   ( v, cs ) <- reads cs
	   ( n, cs ) <- reads cs
	   return ( kante v n, cs )
	 else []
