-- -- $Id$

module Dot.Graph where

-- represents the contents of a .dot file
-- file format: see http://www.graphviz.org/cgi-bin/man?dot

-- restrictions: no subgraphs
-- no default edge and node attributes

-- no graph attributes
-- resp. fixed defaults only

import qualified Dot.Node
import qualified Dot.Edge
import qualified Boxing.Position as B

import ToDoc
import Reader
import Parsec (Parser, option, many)
import Dot.Parsec
import Size

data Type = Type { directed :: Bool 
	       , name	  :: String
	       , nodes	  :: [ Dot.Node.Type ]
	       , edges	  :: [ Dot.Edge.Type ]
	       }

instance Size Type where size = length . nodes

gmap :: ( String -> String ) -> ( Type -> Type )
gmap f g = g { nodes = map (Dot.Node.nmap f) $ nodes g
	     , edges = map (Dot.Edge.emap f) $ edges g
	     }

scale :: Double -> Type -> Type
scale s g = 
    let c = B.Position { B.width = s, B.height = 0 }
        f n = n { Dot.Node.position = fmap (c *) $ Dot.Node.position n }
    in  g { Dot.Graph.nodes = map f $ Dot.Graph.nodes g }
	       

beside :: Type -> Type -> Type
beside l r = 
    let ll = gmap ('L' : ) l
	rr = gmap ('R' : ) r
    in	Type
	{ directed = directed l
	, name = name l ++ name r
	, nodes = nodes ll ++ nodes rr
	, edges = edges ll ++ edges rr
	}

besides :: [ Type ] -> Type
besides = foldr1 beside

instance ToDoc Type where
    toDoc d = 
        let header = text $ case directed d of 
			  False -> "graph"
			  True	-> "digraph" 
	    nm = text ( name d )

	    atts = map text 
		 [ "rankdir = LR"
		 , "center = 1"
		 , "ratio = fill"
		 ]
	    ns  = map toDoc $ nodes d
	    es  = map toDoc $ edges d
	in      hsep [ header,  nm ]
	    <+> braces ( vcat $ punctuate semi $ {- atts ++ -} ns ++ es )

instance Show Type where
    show = render . toDoc

direction 
    =   do my_reserved "graph"   ; return False
    <|> do my_reserved "digraph" ; return True

instance Reader Type where
    reader = do
        f <- direction
	nm <- soi
	my_braces $ do
            atts
	    ts <- many $ do t <- thing ; my_semi ; return t
            return $ Type { directed = f
			  , name = nm
			  , nodes = do Right t <- ts ; return t
			  , edges = do Left  t <- ts ; return t
			  }

atts = do
    option () ( do my_reserved "node" ; args ; my_semi ; return () )
    option () ( do my_reserved "graph"; args ; my_semi ; return () )

args = option () 
     $ my_brackets 
     $ do my_commaSep arg ; return () 

arg = do
    my_identifier
    my_equals
    my_reserved "\"\\N\"" <|> do my_stringLiteral ; return ()
    

thing :: Parser ( Either Dot.Edge.Type Dot.Node.Type )
thing = do
    a <- soi
    fmap Left ( Dot.Edge.continue a ) <|> fmap Right ( Dot.Node.continue a )

instance Read Type where
    readsPrec = parsec_readsPrec




