module GVKnoten ( main ) where

import Char ( isDigit, isSpace, digitToInt )
import System
import GVKnoten.Type
import Graph.Viz 
import Graph.Graph
import Set
import FiniteMap 
import Monad ( guard )
import GVKnoten.Parser (parse14, parse15) 
import Next

main :: IO ()
main = do
	argv <- getArgs
	case argv of
		[]       -> info
--		[ file ] -> start -- start file
		_        -> info

-- hier muss ein anderes instanzTrans her
-- damit bei ID /= Label das Label erhalten bleibt!
instanzTrans :: ShowText knoten => GVTrans knoten
instanzTrans      = GVTrans
	{ getGVProg    = Default
	, getGVFormat  = "png"
	, isGVDirected = True
	, getGVNID     = showText
	, getGVNName   = showText 
	, getGVNLabel  = Nothing 
	, getGVNColor  = Nothing
	, getGVNXAtts  = Nothing
	, getGVELabel  = Nothing
	, getGVEXAtts  = Nothing
	}


paint :: (Show a, ShowText a, Next a, Ord a) => String -> a ->  IO ()
paint fname w = do
    let g = spielbaum w
	-- knoten durchnumerieren
	fm = listToFM $ zip (setToList $ knoten g) [ 1 .. ]
	t = fromMaybe (error "Next.paint.t") . lookupFM fm
    getGraphviz (spielbaum w) (instanzTrans t) fname
    return ()

instanzTrans2 :: ShowText knoten => (knoten -> Int) -> GVTrans knoten
instanzTrans2 t = GVTrans
	{ getGVProg = Default
	, getGVFormat = "png"
	, isGVDirected = True
	, getGVNID = show . t
	, getGVNName = showText -- Knotenname auch
	, getGVNLabel = Nothing
	, getGVNColor = Nothing
	, getGVNXAtts = Nothing
	, getGVELabel = Nothing
	, getGVEXAtts = Nothing
	}

-- out_6 :: (Ord a, Enum a, Num a, ShowText a) => Graph a -> IO ( FiniteMap a GVKnoten )
out_6 :: (ShowText a) => Graph a -> IO ( FiniteMap Int GVKnoten )
out_6 graph = do
	let 
			file    = "007"
			inFile  = file ++ ".dot"
			outFile = file ++ ".tmp"
			command = "dot" ++ " -o " ++ outFile ++ " " ++ inFile
	( _ , _ , exitCode ) <- getGraphviz ( graph ) instanzTrans file
	-- guard ( exitCode == ExitSuccess )
	exitCode <- system command
	i <- readFile outFile
 	return ( listToFM $ zip [1..] (parse14 i) )

-- out :: (Ord a, Enum a, Num a, ShowText a) => Graph a -> IO ( FiniteMap a GVKnoten )
out_13 :: (Ord a, ShowText a) => Graph a -> [ a ] -> IO ( FiniteMap a GVKnoten )
out_13 graph a_lst = do
	let 
			file    = "007"
			inFile  = file ++ ".dot"
			outFile = file ++ ".tmp"
			command = "dot" ++ " -o " ++ outFile ++ " " ++ inFile
	( _ , _ , exitCode ) <- getGraphviz ( graph ) instanzTrans file
	-- guard ( exitCode == ExitSuccess )
	exitCode <- system command
	i <- readFile outFile
 	return ( listToFM $ zip a_lst (parse14 i) )

-- so soll es eigentlich aussehen!!
-- aber ich habe ein Problem mit der Liste vom allgemeinen Typ
{-
graph_to_FM_a_gvnode :: Graph a -> IO ( FiniteMap a GVKnoten )
graph_to_FM_a_gvnode graph = do
	let 
			file    = "007"
			inFile  = file ++ ".dot"
			outFile = file ++ ".tmp"
			command = "dot" ++ " -o " ++ outFile ++ " " ++ inFile
	( _ , _ , exitCode ) <- getGraphviz ( graph ) instanzTrans file
	-- guard ( exitCode == ExitSuccess )
	exitCode <- system command
	i <- readFile outFile
 	return ( listToFM $ zip [..] (parse14 i) )
-}

out_14 :: IO ()
out_14 = do
	( _ , _ , exitCode ) <- getGraphviz test_graph_int instanzTrans "123"
	-- guard ( (fst exitCode) == ExitSuccess ) putStrLn "asdf"
	-- guard ( (exitCode) == ExitSuccess )
	putStrLn ( "#" ++ "okay" ++ "#" )

out_11 :: IO ()
out_11 = do
	fm <- out_13 test_graph_int [1..]
	putStr $ showFiniteMap fm

out_8 :: (Ord a, Enum a, Num a, ShowText a) => Graph a -> IO ( FiniteMap a GVKnoten )
out_8 graph = do
	let 
			file    = "021"
			inFile  = file ++ ".dot"
			outFile = file ++ ".tmp"
			command = "dot" ++ " -o " ++ outFile ++ " " ++ inFile
	getGraphviz ( graph ) instanzTrans file
	system command
	parsed_things <- parse15 outFile
	return ( listToFM $ zip [1..] parsed_things )

showFiniteMap :: (Show a, Show b) => FiniteMap a b -> String 
showFiniteMap fm = "listToFM" ++ show (fmToList fm)

info :: IO ()
info = do
	putStrLn "hugs aufrufen mit:"
	putStrLn "hugs -98 +o -P$<full_path>/autotool/GVKnoten/:../:../util: GVKnoten.hs"
	putStrLn "und dann graph_to_FM_a_gvnode <graph> aufrufen"

test_graph_int :: Graph Int
test_graph_int = Graph (mkSet[0, 1, 2])	
                       (mkSet[Kante 0 1, Kante 0 2]) 

test_graph_str :: Graph String
test_graph_str = Graph (mkSet["0", "1", "2"])
                       (mkSet[Kante "0" "1", Kante "0" "2", Kante "0" "3"
						 	        ,Kante "2" "4", Kante "2" "5", Kante "3" "6"]) 
