module GVKnoten ( main ) where

import System          -- system command
import GVKnoten.Parser -- (parse14, parse15) 
import GVKnoten.Type   -- Definition von GVKnoten
import Graph.Graph     -- Definition von Graph
import Graph.Viz       -- getGraphviz
import Data.FiniteMap       -- alle FM-Sachen
-- import InstanzTrans    -- myTrans
-- import Testgraph       -- test_graph_int, test_graph_str, test_graph_stt
import CPUTime

main :: IO ()
main = do
	argv <- getArgs
	case argv of
		_        -> GVKnoten.info


test_graph_tre = undefined :: Graph Int
myTrans = undefined

test :: IO ()
test = do
	fm <- graph_to_FM_a_gvnode test_graph_tre
	putStr $ showFiniteMap fm


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
