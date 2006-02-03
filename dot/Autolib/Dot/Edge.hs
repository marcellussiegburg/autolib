module Autolib.Dot.Edge where

-- -- $Id$

import Autolib.ToDoc
import Autolib.Reader

import Text.ParserCombinators.Parsec (option, Parser)
import Autolib.Dot.Parsec

import Data.Maybe ( maybeToList )


-- | implement only those attribs that would be needed
-- for drawing finite automata (= directed edge-labelled graphs)

data Type = Type { from	      :: String
		 , to	      :: String
		 , directed  :: Bool
		 , label :: Maybe String
	         , edge_style :: Maybe String
	         , edge_length :: Maybe String
		 , headlabel :: Maybe String
		 , taillabel :: Maybe String
		 , color :: Maybe String
		 }

emap :: ( String -> String ) -> ( Type -> Type )
emap f e = e { from = f $ from e , to = f $ to e }

blank :: Type
blank = Type {  from = error "Dot.Edge.from"
		 , to = error "Dot.Edge.to"
		, color = Nothing
		 , label = Nothing
		 , headlabel = Nothing
		 , taillabel = Nothing
		 , directed = True -- default (?)
		 , edge_style = Nothing
		 , edge_length = Nothing
		 }


instance ToDoc Type where
    toDoc n = hsep [ text (from n)
		   , text ( if directed n then "->" else "--" )
		   , text (to n) 
		   ]
	<+> brackets ( fsep $ punctuate comma $ do
	    ( name, fun ) <- [ ("label", fmap show . label)
			     , ("headlabel", fmap show . headlabel)
			     , ("taillabel", fmap show . taillabel)
			     , ("style", edge_style)
			     , ("color", color)
			     , ("len", edge_length)
			     ]
	    val <- maybeToList $ fun n
	    return $ text name <+> equals <+> text ( val )
          )

instance Reader Type where
    -- ignoriert ziemlich viel
    reader = do
        a <- soi
        continue a

continue a = do
        f <- direction
	b <- soi
	args <- option [] $ my_brackets $ my_commaSep param
        return $ blank { from = a, to = b, directed = f }

param :: Parser ()
param = do
    name <- my_identifier
    my_equals
    val <- soi
    return ()

direction :: Parser Bool
direction 
    =   do my_reserved "--" ; return False
    <|> do my_reserved "->" ; return True

escape :: String -> String
escape ( '"' : cs ) = show $ init $ cs
escape cs = show cs

	     

