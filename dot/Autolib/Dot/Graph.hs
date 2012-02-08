-- | represents the contents of a .dot file
-- file format: see <http://www.graphviz.org/cgi-bin/man?dot>

-- restrictions: no subgraphs
-- no default edge and node attributes

-- no graph attributes
-- resp. fixed defaults only


module Autolib.Dot.Graph where


import qualified Autolib.Dot.Node
import qualified Autolib.Dot.Attribute
import qualified Autolib.Dot.Edge
import qualified Autolib.Boxing.Position as B

import Autolib.ToDoc
import Autolib.Reader
import Text.ParserCombinators.Parsec (Parser, option, many)
import Autolib.Dot.Parsec
import Autolib.Size

data Type = Type { directed :: Bool 
	       , name	    :: String
	       , nodes	    :: [ Autolib.Dot.Node.Type ]
	       , edges	    :: [ Autolib.Dot.Edge.Type ]
	       , attributes :: [ Autolib.Dot.Attribute.Type ]
	       }

instance Size Type where size = length . nodes

gmap :: ( String -> String ) -> ( Type -> Type )
gmap f g = g { nodes = map (Autolib.Dot.Node.nmap f) $ nodes g
	     , edges = map (Autolib.Dot.Edge.emap f) $ edges g
	     }

scale :: Double -> Type -> Type
scale s g = 
    let c = B.Position { B.width = s, B.height = 0 }
        f n = n { Autolib.Dot.Node.position = 
		      fmap (c *) $ Autolib.Dot.Node.position n 
		}
    in  g { Autolib.Dot.Graph.nodes = map f $ Autolib.Dot.Graph.nodes g }
	       

beside :: Type -> Type -> Type
beside l r = 
    let ll = gmap ('L' : ) l
	rr = gmap ('R' : ) r
    in	Type
	{ directed = directed l
	, name = name l ++ name r
	, nodes = nodes ll ++ nodes rr
	, edges = edges ll ++ edges rr
	, attributes = []
	}

besides :: [ Type ] -> Type
besides = foldr1 Autolib.Dot.Graph.beside

instance ToDoc Type where
    toDoc d = 
        let header = text $ case directed d of 
			  False -> "graph"
			  True	-> "digraph" 
	    nm = text ( name d )
	    atts = map toDoc $ attributes d
	    ns  = map toDoc $ nodes d
	    es  = map toDoc $ edges d
	in      hsep [ header, nm ]
	    <+> braces ( vcat $ punctuate semi $ atts ++ ns ++ es )

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
			  , attributes = []
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
    

thing :: Parser ( Either Autolib.Dot.Edge.Type Autolib.Dot.Node.Type )
thing = do
    a <- soi
    fmap Left ( Autolib.Dot.Edge.continue a ) <|> fmap Right ( Autolib.Dot.Node.continue a )





