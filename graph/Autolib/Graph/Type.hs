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
-- ToDoc implementation for Graph and Kante
instance (ToDoc a, ToDoc (Set a)) => ToDoc (Graph a) where
    toDoc g = text "Graph" <+> braces (
      fsep $ punctuate comma 
      [ text "knoten " <+> equals <+> toDoc (knoten g)
      , text "kanten " <+> equals <+> toDoc (kanten g)
      ])
      
instance ToDoc a => ToDoc (Kante a) where
{-
    toDoc k = text "Kante" <+> braces ( 
      fsep $ punctuate comma
      [ text "von" <+> equals <+> toDoc (von k)
      , text "nach" <+> equals <+> toDoc (nach k)
      ])
-}
    toDoc k = text "kante" <+> toDoc (von k) <+> toDoc (nach k)


instance ToDoc (Graph a) => Show (Graph a) where
{-    showsPrec p g = showString $ unlines 
		  $ [ "{ knoten = " ++ show (setToList (knoten g))
		    , ", kanten = " ++ show (setToList (kanten g))
		    , "}"
		    ]
-}    
  show = render . toDoc
 
  
instance (ToDoc a) => Show (Kante a) where
--    showsPrec p k = showString $ "(" ++ show (von k) ++ "," ++ show (nach k) ++ ")"
  show = render . toDoc

