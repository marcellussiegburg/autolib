module Dot.Edge where

-- -- $Id$

-- implement only those attribs that would be needed
-- for drawing finite automata (= directed edge-labelled graphs)

import ToDoc
import Reader

import Text.ParserCombinators.Parsec (option, Parser)
import Dot.Parsec

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
	    return $ text name <+> equals <+> text ( escape $ val )
          )

instance Show Type where
    show = render . toDoc

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
escape cs = show $ tail $ init $ cs
	     

