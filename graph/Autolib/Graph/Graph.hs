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
import ToDoc

-------------------------------------------------------------------------------

data Graph a  = Graph
	      { knoten    :: Set a
	      , kanten    :: Set (Kante a)
	      } deriving (Read)


data Kante a  = Kante
	      { von       :: a
	      , nach      :: a
        } deriving (Eq, Ord)

kante :: a -> a -> (Kante a)
kante x y = Kante { von = x, nach = y }


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

instance Read a => Read (Kante a) where
    readsPrec p cs = do
        ( k, cs ) <- lex cs
	if k == "Kante" then do
	   -- dieser Zwei deprecated
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
