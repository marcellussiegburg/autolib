-- | the picture contains to kinds of nodes:
-- state nodes and transition nodes
--
-- a transition node q0 -> c(q1, .., qn) 
-- is labelled with c
-- and has an incoming edge from the state node q0
-- and outgoing edges to state nodes qi
--
-- note that it is important to keep the ordering for outgoing edges
-- or else it is necessary to label them (by their argument position) 

module NFTA.Dot where

--  $Id$

import NFTA.Type
import Dot.Dot

import qualified Dot.Graph
import qualified Dot.Node
import qualified Dot.Edge
import qualified Dot.Arrange

import Data.Set
import Data.FiniteMap

instance ( NFTAC c s , Show s, Show c )
     => ToDot ( NFTA c s ) where
    toDot a = helper (numeric a) a
    toDotProgram a = "dot" -- "neato"
    toDotOptions a = "-Grankdir=LR" -- "-s"

numeric :: NFTAC c s
	=> NFTA c s
	-> ( s -> String )
numeric a = 
    let fm = listToFM $ zip (lstates a) $ map show [ 0 :: Int .. ] 
    in  fromMaybe (error "NFTA.Dot.numeric") . lookupFM fm


helper num a = 
        let tricky cs = 
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
			   { Dot.Node.ident = num p
			   , Dot.Node.label = Just $ show p
			   , Dot.Node.shape = Just sh
			   }


	    -- unsichtbare knoten (für start-pfeile)
	    uns = do p <- lstarts a
		     return $ Dot.Node.blank
			   { Dot.Node.ident = "U" ++ num p 
			   , Dot.Node.node_style = Just "invis"
			   }
    
	    -- tatsächliche zustandsübergänge
	    es = do ( p, x, q ) <- unCollect $ trans a
		    return $ Dot.Edge.blank
			   { Dot.Edge.from  = num p
			   , Dot.Edge.to    = num q
			   , Dot.Edge.taillabel = Just $ quoted $ tricky $ show x
			   }
	    -- start-pfeile
	    ss = do p <- lstarts a
		    return $ Dot.Edge.blank
			   { Dot.Edge.from  = "U" ++ num p 
			   , Dot.Edge.to    = num p
			   }

	in  Dot.Graph.Type 
	    { Dot.Graph.directed = True
	    , Dot.Graph.name = "NFA"
	    , Dot.Graph.nodes = ns ++ uns
	    , Dot.Graph.edges = es ++ ss
	    }
