-- $Id$

module Dot.Graph where

-- represents the contents of a .dot file
-- file format: see http://www.graphviz.org/cgi-bin/man?dot

-- restrictions: no subgraphs
-- no default edge and node attributes
-- no graph attributes

import qualified Dot.Node
import qualified Dot.Edge

import ToDoc

data Graph = Graph { directed :: Bool 
	       , name	  :: String
	       , nodes	  :: [ Dot.Node.Type ]
	       , edges	  :: [ Dot.Edge.Type ]
	       }

instance ToDoc Graph where
    toDoc d = 
        let header = text $ case directed d of 
			  False -> "graph"
			  True	-> "digraph" 
	    nm = text ( name d )
	    ns  = map toDoc $ nodes d
	    es  = map toDoc $ edges d
	in      hsep [ header,  nm ]
	    <+> braces ( vcat $ punctuate semi $ ns ++ es )

instance Show Graph where
    show = render . toDoc




