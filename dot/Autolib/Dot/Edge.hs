module Dot.Edge where

-- $Id$

-- implement only those attribs that would be needed
-- for drawing finite automata (= directed edge-labelled graphs)

import ToDoc
import Maybe ( maybeToList )


data Type = Type { from	      :: String
		 , to	      :: String
		 , label :: Maybe String
		 , headlabel :: Maybe String
		 , taillabel :: Maybe String
		 , directed  :: Bool
		 }

emap :: ( String -> String ) -> ( Type -> Type )
emap f e = e { from = f $ from e , to = f $ to e }

blank :: Type
blank = Type {  from = error "Dot.Edge.from"
		 , to = error "Dot.Edge.to"
		 , label = Nothing
		 , headlabel = Nothing
		 , taillabel = Nothing
		 , directed = True -- default (?)
		 }


instance ToDoc Type where
    toDoc n = hsep [ text (from n)
		   , text ( if directed n then "->" else "--" )
		   , text (to n) 
		   ]
	<+> brackets ( fsep $ punctuate comma $ do
	    ( name, fun ) <- [ ("label", label)
			     , ("headlabel", headlabel)
			     , ("taillabel", taillabel)
			     ]
	    val <- maybeToList $ fun n
	    return $ text name <+> equals <+> text val
          )

instance Show Type where
    show = render . toDoc


