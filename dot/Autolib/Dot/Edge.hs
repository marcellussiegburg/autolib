module Dot.Edge where

-- implement only those attribs that would be needed
-- for drawing finite automata (= directed edge-labelled graphs)

import ToDoc
import Maybe ( maybeToList )


data Type = Type { from	      :: String
		 , to	      :: String
		 , label :: Maybe String
		 , headlabel :: Maybe String
		 , taillabel :: Maybe String
		 }

blank :: Type
blank = Type {  from = error "Dot.Edge.from"
		 , to = error "Dot.Edge.to"
		 , label = Nothing
		 , headlabel = Nothing
		 , taillabel = Nothing
		 }


instance ToDoc Type where
    toDoc n = hsep [ text (from n), text "->", text (to n) ]
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


