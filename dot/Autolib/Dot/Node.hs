module Dot.Node where

-- implement only those attribs that would be needed
-- for drawing finite automata (= directed edge-labelled graphs)

import ToDoc hiding ( style )
import Maybe ( maybeToList )

data Type = Type { ident :: String
		 , label :: Maybe String
		 , shape :: Maybe String
		 , color :: Maybe String
		 , style :: Maybe String
		 }

blank :: Type
blank =  Type {  ident = error "Dot.Edge.ident"
		 , label = Nothing
		 , shape = Just "circle"
		 , color = Nothing
		 , style = Nothing
		 }


instance ToDoc Type where
    toDoc n = text (ident n) <+> brackets ( fsep $ punctuate comma $ do
	    ( name, fun ) <- [ ("label", label)
			     , ("shape", shape)
			     , ("color", color)
			     , ("style", style)
			     ]
	    val <- maybeToList $ fun n
	    return $ text name <+> equals <+> toDoc val
          )

instance Show Type where
    show = render . toDoc

