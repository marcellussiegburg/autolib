module GVKnoten.Type where

data GVKnoten = GVKnoten { ident  :: String
			    , label  :: String
			    , pos    :: (Int, Int)
			    , width  :: Float
   			    , height :: Float
			 } deriving (Eq, Ord, Show)
