-- this is a Main file

-- -- $Id$

import Multi.Chooser
import Multi.Generator
import Multi.Config

import Graph.Generate
import Util.Datei

import Exception
import System

conf :: Config
conf = Config { prefix = Datei { pfad = [ "graph", "quiz" ] 
                               , name = "none"
			       , relativzahl = 0
			       }
		      , objects = 2
		      , names = 3 
		      , tries = 2
		      }
    
main :: IO ()
main = do

    -- füge ein paar neue Graphen hinzu // jedesmal ??
    gs <- sequence $ replicate 10 $ some 7 2
    generator ( prefix conf ) gs

    -- nun raten
    chooser conf
 `Exception.catch` \ any -> do
     putStrLn $ "exception: " ++ show any
