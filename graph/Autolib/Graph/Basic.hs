module Autolib.Graph.Basic where

import Autolib.Graph.Graph hiding ( union )
import Autolib.Graph.Display

import Autolib.Boxing

import qualified Autolib.Set as Set
import qualified Data.List as List
import Autolib.FiniteMap
import Data.Maybe
import Control.Monad ( guard )
import Autolib.ToDoc hiding ( empty )

independent :: GraphC a => Set a -> Graph a
independent = independent0 . setToList

-- | klebt zusammen (zeichnet übereinander!)
independent0 :: GraphC a => [a] -> Graph a
independent0 xs = Graph 
    { graph_info = text $ "E_" ++ show (length xs)
    , show_labels = False
    , graph_texinfo = "{E_{" ++ show (length xs) ++ "}}"
    , knoten = mkSet xs
    , kanten = emptySet
    , graph_layout = listToFM $ do
          let n = length xs
	  (i, x) <- zip [0 ..] $ xs
	  let phi = 2 * fromIntegral i * pi / fromIntegral n
	  return ( x
		 , Position { width  = 1 + cos phi , height = 1 + sin phi } 
		 )
    , bounding = Position { width = 2, height = 2 }
    , layout_hints = [ "-s" ]
    , layout_program = Neato
    }

clique :: GraphC a => Set a -> Graph a
clique = clique0 . setToList

clique0 :: GraphC a => [a] -> Graph a
clique0 xs = ( independent0 xs )
	    { graph_info = text $ "K_" ++ show (length xs)
            , graph_texinfo = "{K_{" ++ show (length xs) ++ "}}"
	    , kanten = mkSet $ do 
			 (x : ys) <- List.tails $ xs
			 y <- ys
			 return $ kante x y
	    }

empty :: GraphC a => Graph a
empty = clique $ Set.emptySet

path :: GraphC a => [ a ] -> Graph a
path xs = 
    Graph { graph_info = text $ "P_" ++ show (length xs)
            , graph_texinfo = "{P_{" ++ show (length xs) ++ "}}"
	  , show_labels = False
	  , knoten = mkSet xs
	  , kanten = mkSet $ do 
	       (u , v ) <- zip xs ( tail xs )
	       return $ kante u v
	  , graph_layout = listToFM $ do
	       let w = 1 / fromIntegral (length xs)
	       (i, x) <- zip [0..] $ xs
	       return ( x , Position { width = w, height = 0 } )
	  , bounding = 1
    , layout_hints = [ "-s" ]
    , layout_program = Neato
	  }

circle :: GraphC a => [ a ] -> Graph a
circle xs = ( independent0 xs )
	    { graph_info = text $ "C_" ++ show (length xs)
            , graph_texinfo = "{C_{" ++ show (length xs) ++ "}}"
	    , kanten = mkSet $ do 
			 (x, y) <- zip xs ( last xs : xs )
			 guard $ x /= y
			 return $ kante x y
	    }


