module Autolib.Dot.Node where

--  $Id$

-- implement only those attribs that would be needed
-- for drawing finite automata (= directed edge-labelled graphs)

import Autolib.Boxing.Position

import Autolib.ToDoc
import Autolib.Reader 
import Text.ParserCombinators.Parsec ( option, Parser )
import Autolib.Dot.Parsec

import Data.Maybe ( maybeToList )

data Type = Type { ident :: String
		 , label :: Maybe String
		 , shape :: Maybe String
		 , color :: Maybe String
		 , node_style :: Maybe String
		 , pinned :: Maybe Bool
		 , position :: Maybe Position
		 }

nmap :: ( String -> String ) -> ( Type -> Type )
nmap f n = n { ident = f $ ident n }

blank :: Type
blank =  Type {  ident = error "Dot.Node.ident"
		 , label = Nothing
		 , shape = Nothing
		 , color = Nothing
		 , node_style = Nothing
		 , pinned = Nothing
		 , position = Nothing
		 }


instance ToDoc Type where
    toDoc n = text (ident n) <+> brackets ( fsep $ punctuate comma $ do
	    ( name, fun ) <- [ ("label", label)
			     , ("shape", shape)
			     , ("color", color)
			     , ("style", node_style)
			     , ("pos", fmap pos_show . position)
			     , ("pin", fmap pin_show . pinned)
			     ]
	    val <- maybeToList $ fun n
	    return $ text name <> equals <> toDoc val
          )

instance Reader Type where
    reader = do
        i <- soi
        continue i

continue i = do
	params <- option [] $ my_brackets $ my_commaSep param
        return $ foldr ($) ( blank { ident = i } ) params

-- | erstmal sehr simplified, erkennt nur positionen
param :: Parser ( Type -> Type )	
param = 
        do my_reserved "pos"   ; my_equals ; a <- soi 
	   return ( \ x -> x { position = Just $ lies a } )
    <|> do my_identifier ; my_equals ; a <- soi
	   return ( id )

    

pos_show :: Autolib.Boxing.Position.Position -> String
pos_show = Autolib.Boxing.Position.zeige

pin_show :: Bool -> String
pin_show f = case f of
    True -> "true"
    False -> "false"

instance Show Type where
    show = render . toDoc

