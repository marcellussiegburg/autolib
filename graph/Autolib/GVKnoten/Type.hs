module GVKnoten.Type where

-- $Id$

type Pos = ( Int, Int )

data GVKnoten = GVKnoten { ident  :: String
			    , label  :: String
			    , pos    :: Pos
			    , width  :: Float
   			    , height :: Float
			 } deriving (Eq, Ord, Show)
