module Autolib.Graph.Generate 

( some )

where

-- -- $Id$

import Autolib.Graph.Graph hiding ( union )
import Autolib.Graph.Basic
import Autolib.Graph.Ops

import Autolib.Util.Generate
import Autolib.Util.Zufall

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
      , depth = error "Autolib.Graph.Generate.conf.depth"
      , size = error "Autolib.Graph.Generate.conf.size"
      }

some :: Int -> Int -> IO (Graph Int)
some s d = generate $ conf { size = s, depth = d }


