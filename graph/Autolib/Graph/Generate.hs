module Graph.Generate where

-- $Id$

import Graph.Type hiding ( empty , union )
import Graph.Basic
import Graph.Ops

import Util.Generate
import Util.Zufall

import Random

conf :: Config ( Graph Int )
conf  = Config 
      { base = [ \ s -> return $ independent $ mkSet [ 1 .. s ]
               , \ s -> return $ clique      $ mkSet [ 1 .. s ]
               , \ s -> return $ path        $       [ 1 .. s ]
               , \ s -> return $ circle      $       [ 1 .. s ]
	       ]
      , ops  = [ ( 1 , \ [ g ]    ->        complement g   )
	       , ( 2 , \ [ l, r ] -> normalize $ union l r )
	       , ( 2 , \ [ l, r ] -> normalize $ times l r )
	       -- , ( 2 , \ [ l, r ] -> normalize $ grid  l r )
	       ]
      }

some :: Int -> Int -> IO (Graph Int)
some s d = generate $ conf { size = s, depth = d }


