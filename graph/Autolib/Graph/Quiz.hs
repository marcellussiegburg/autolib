-- this is a Main file

-- $Id$

import Multi.Chooser
import Multi.Generator
import Multi.Config

import Graph.Generate
import Util.Datei

conf :: Config
conf = Config { prefix = Datei { pfad = [ "graph", "quiz" ] }
		      , objects = 5
		      , names = 3 
		      , tries = 5
		      }
    
main :: IO ()
main = do

    -- füge ein paar neue Graphen hinzu // jedesmal ??
    gs <- sequence $ replicate 10 $ some 10 4
    generator ( prefix conf ) gs

    -- nun raten
    chooser conf
