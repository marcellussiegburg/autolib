module Dot.Node where

-- $Id$

-- implement only those attribs that would be needed
-- for drawing finite automata (= directed edge-labelled graphs)

import ToDoc

import Maybe ( maybeToList )

data Type = Type { ident :: String
		 , label :: Maybe String
		 , shape :: Maybe String
		 , color :: Maybe String
		 , node_style :: Maybe String
		 }

nmap :: ( String -> String ) -> ( Type -> Type )
nmap f n = n { ident = f $ ident n }

blank :: Type
blank =  Type {  ident = error "Dot.Edge.ident"
		 , label = Nothing
		 , shape = Just "circle"
		 , color = Nothing
		 , node_style = Nothing
		 }


instance ToDoc Type where
    toDoc n = text (ident n) <+> brackets ( fsep $ punctuate comma $ do
	    ( name, fun ) <- [ ("label", label)
			     , ("shape", shape)
			     , ("color", color)
			     , ("style", node_style)
			     ]
	    val <- maybeToList $ fun n
	    return $ text name <+> equals <+> toDoc val
          )

instance Show Type where
    show = render . toDoc

