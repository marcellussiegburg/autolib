-- $Id$

module Dot.Graph where

-- represents the contents of a .dot file
-- file format: see http://www.graphviz.org/cgi-bin/man?dot

-- restrictions: no subgraphs
-- no default edge and node attributes

-- no graph attributes
-- resp. fixed defaults only

import qualified Dot.Node
import qualified Dot.Edge

import ToDoc

data Type = Type { directed :: Bool 
	       , name	  :: String
	       , nodes	  :: [ Dot.Node.Type ]
	       , edges	  :: [ Dot.Edge.Type ]
	       }

gmap :: ( String -> String ) -> ( Type -> Type )
gmap f g = g { nodes = map (Dot.Node.nmap f) $ nodes g
	     , edges = map (Dot.Edge.emap f) $ edges g
	     }

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




