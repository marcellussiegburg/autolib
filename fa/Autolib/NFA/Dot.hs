-- -- $Id$

module Autolib.NFA.Dot

( module Autolib.Dot.Dot
, toDot_layered
)

where

import Autolib.NFA.Type
import Autolib.Dot.Dot

import qualified Autolib.Dot.Graph
import qualified Autolib.Dot.Node
import qualified Autolib.Dot.Edge
import qualified Autolib.Dot.Arrange

import Autolib.ToDoc

import Data.Set
import Autolib.FiniteMap
import Data.Maybe
import Data.List (inits)

numeric :: NFAC c a 
	=> NFA c a 
	-> ( a -> String )
numeric a = 
    let fm = listToFM $ zip (lstates a) $ map show [ 0 :: Int .. ] 
    in  fromMaybe (error "NFA.Dot.numeric") . lookupFM fm

toDot_layered :: ( NFAC c a , Show a, Show c )
	      => NFA c a 
	      -> [ Set a ] 
              -> IO Autolib.Dot.Graph.Type
toDot_layered a xss = do

    let num = numeric a
        d = helper num a
        yss = do
	      pre <- inits $ map (smap num) xss
	      return $ unionManySets pre
    Autolib.Dot.Arrange.layered d yss

    
-- zustände werden mit [0 .. ] durchnumeriert
-- akzeptierende zustände bekommen doppelkreis drumrum
-- startzustände bekommen pfeil dran,
-- dieser kommt aus unsichtbarem zustand mit idents U0, U1, ..

instance ( NFAC c a , Show a, Show c )
     => ToDot ( NFA c a ) where
    toDot a = helper (numeric a) a
    toDotProgram a = "dot" -- "neato"
    toDotOptions a = "-Grankdir=LR" -- "-s"

helper num a = 
        let 

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
		    return $ Autolib.Dot.Node.blank
			   { Autolib.Dot.Node.ident = num p
			   , Autolib.Dot.Node.label = case show p of
			            "" -> Nothing ; cs -> Just cs
			   , Autolib.Dot.Node.shape = case show p of
			            "" -> Nothing ; cs -> Just sh
			   }

	    -- unsichtbare knoten (für start-pfeile)
	    uns = do p <- lstarts a
		     return $ Autolib.Dot.Node.blank
			   { Autolib.Dot.Node.ident = "U" ++ num p 
			   , Autolib.Dot.Node.node_style = Just "invis"
			   }
    
	    -- tatsächliche zustandsübergänge
	    es = do ( p, x, q ) <- unCollect $ trans a
		    return $ Autolib.Dot.Edge.blank
			   { Autolib.Dot.Edge.from  = num p
			   , Autolib.Dot.Edge.to    = num q
			   , Autolib.Dot.Edge.taillabel = Just $ quoted $ tricky $ show x
			   }
	    -- start-pfeile
	    ss = do p <- lstarts a
		    return $ Autolib.Dot.Edge.blank
			   { Autolib.Dot.Edge.from  = "U" ++ num p 
			   , Autolib.Dot.Edge.to    = num p
			   }

	in  Autolib.Dot.Graph.Type 
	    { Autolib.Dot.Graph.directed = True
	    , Autolib.Dot.Graph.name = "NFA"
	    , Autolib.Dot.Graph.nodes = ns ++ uns
	    , Autolib.Dot.Graph.edges = es ++ ss
	    }


