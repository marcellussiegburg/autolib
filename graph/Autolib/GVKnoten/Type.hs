module GVKnoten.Type where

-- $Id$

type Pos = ( Int, Int )

-- das wird gelesen
data GVKnoten = GVKnoten { ident  :: String
			    , label  :: String
			    , pos    :: Pos
			    , width  :: Float
   			    , height :: Float
			 } deriving (Eq, Ord, Show)
