module GVKnoten ( main ) where

import System          -- system command
import GVKnoten.Parser -- (parse14, parse15) 
import GVKnoten.Type   -- Definition von GVKnoten
import Graph.Graph     -- Definition von Graph
import Graph.Viz       -- getGraphviz
import FiniteMap       -- alle FM-Sachen
-- import InstanzTrans    -- myTrans
-- import Testgraph       -- test_graph_int, test_graph_str, test_graph_stt
import CPUTime

main :: IO ()
main = do
	argv <- getArgs
	case argv of
		_        -> info


test_graph_tre = undefined :: Graph Int
myTrans = undefined

test :: IO ()
test = do
	fm <- graph_to_FM_a_gvnode test_graph_tre
	putStr $ showFiniteMap fm

-- Ziel:     Graph a -> IO ( FiniteMap a GVKnoten )
-- Problem:  xs = <unendliche Liste fehlt>::[a] 
graph_to_FM_a_gvnode :: (ShowText a, Ord a) 
		=> Graph a -> IO ( FiniteMap a GVKnoten )
graph_to_FM_a_gvnode graph = do
	let 
			file    = "022"
			inFile  = file ++ ".dot"
			outFile = file ++ ".tmp"
			command = "dot" ++ " -o " ++ outFile ++ " " ++ inFile
 			xs      = setToList $ knoten graph
 	( _ , _ , exitCode ) <- getGraphviz ( graph ) myTrans file
	if exitCode == ExitSuccess
		then do
			system command
			parsed_things <- parse outFile
--			system ("rm " ++ file ++ ".tmp")
			system ("rm " ++ file ++ ".dot")
			return ( listToFM $ zip xs $ parsed_things )
		else error "Problem mit dem Graph-Viz"

showFiniteMap :: (Show a, Show b) => FiniteMap a b -> String 
showFiniteMap fm = "listToFM" ++ show (fmToList fm)

info :: IO ()
info = do
	putStrLn "hugs aufrufen mit:"
	putStrLn "hugs -98 +o -P$<full_path>/autotool/GVKnoten/:../:../util/: GVKnoten.hs"
	putStrLn "Module unter theopc.informatik.uni-leipzig.de/~challenger???"
	putStrLn "und dann graph_to_FM_a_gvnode <graph> aufrufen"

time :: IO ()
time = do
	before <- getCPUTime
	x <- test
	after <- getCPUTime
	putStrLn ("\n\n")
	putStrLn $ show (((read(show(after - before)))::Float)/(1.0e+12))
