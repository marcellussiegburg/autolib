-- $Id$

module NFA.Dot

( module Dot.Dot
)

where

import NFA.Type
import Dot.Dot

import qualified Dot.Graph
import qualified Dot.Node
import qualified Dot.Edge

import Set
import FiniteMap
import Maybe
import ToDoc

-- zustände werden mit [0 .. ] durchnumeriert
-- akzeptierende zustände bekommen doppelkreis drumrum
-- startzustände bekommen pfeil dran,
-- dieser kommt aus unsichtbarem zustand mit idents U0, U1, ..

instance ( NFAC c a , Show a, Show c )
     => ToDot ( NFA c a ) where
    toDot a = 
        let fm = listToFM $ zip (lstates a) $ [ 0 .. ] 
	    num = fromMaybe (error "NFA_Dot.num") . lookupFM fm

	    tricky cs = 
	        if take 1 cs `elem` [ "\"", "'" ]  
		   -- dann ist es Show String|Char
		then tail ( init cs )	  -- und eine "-klammer kann weg
		else cs
	    quoted cs = "\"" ++ cs ++ "\""


	    -- tatsächliche knoten (zustände)
	    ns = do p <- lstates a
		    let sh = case p `elementOf` finals a of
			      True  -> "doublecircle"
			      False -> "ellipse"
		    return $ Dot.Node.blank
			   { Dot.Node.ident = show $ num p
			   , Dot.Node.label = Just $ show p
			   , Dot.Node.shape = Just sh
			   }


	    -- unsichtbare knoten (für start-pfeile)
	    uns = do p <- lstarts a
		     return $ Dot.Node.blank
			   { Dot.Node.ident = "U" ++ show ( num p )
			   , Dot.Node.node_style = Just "invis"
			   }
    
	    -- tatsächliche zustandsübergänge
	    es = do ( p, x, q ) <- unCollect $ trans a
		    return $ Dot.Edge.blank
			   { Dot.Edge.from  = show $ num p
			   , Dot.Edge.to    = show $ num q
			   , Dot.Edge.taillabel = Just $ quoted $ tricky $ show x
			   }
	    -- start-pfeile
	    ss = do p <- lstarts a
		    return $ Dot.Edge.blank
			   { Dot.Edge.from  = "U" ++ show ( num p )
			   , Dot.Edge.to    = show $ num p
			   }

	in  Dot.Graph.Type 
	    { Dot.Graph.directed = True
	    , Dot.Graph.name = "NFA"
	    , Dot.Graph.nodes = ns ++ uns
	    , Dot.Graph.edges = es ++ ss
	    }


