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




