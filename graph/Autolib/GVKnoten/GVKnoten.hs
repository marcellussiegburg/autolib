module GVKnoten ( main ) where

-- import FiniteMap
import Char ( isDigit, isSpace, digitToInt )
import System
-- import Set
-- import Pretty
-- import Graph.Gr
import Graph.Viz 
-- import Monad ( guard )

-------------------------------------------------------------------------------
import Graph.Graph
import Set
import FiniteMap -- deriving Show
import Monad ( guard )
-------------------------------------------------------------------------------
{- data GVKnoten = GVKnoten { ident  :: String
							    , label  :: String
							    , pos    :: (Int, Int)
							    , width  :: Float
   							 , height :: Float
							    } deriving (Eq, Ord, Show)-}

main :: IO () -- ExitCode -- String 
main = do
	argv <- getArgs
	case argv of
		[]       -> info
		[ file ] -> start file
		_        -> info

-- nextTry :: Graph a -> [ GVKnoten ]
-- nextTry graph = do
	-- getGraphviz ( graph ) instanzTrans "011"  -- erzeugt das file "011.dot"
	-- return (parse_line ptl1)

-- the_only_useful_thing :: Graph a -> IO ( FiniteMap a GVKnoten ) 
-- the_only_useful_thing graph = getGraphviz ( graph ) instanzTrans "011"
-- :t getGraphviz = IO (String, GVFormat, ExitCode)

test_graph :: Graph Int
test_graph = Graph (mkSet[0, 1, 2])
                   (mkSet[Kante 0 1, Kante 0 2]) --" "2", Kante "0" "3"
						 	--	 ,Kante "2" "4", Kante "2" "5", Kante "3" "6"]) 

-- auto_label :: IO (String, GVFormat, ExitCode)
-- auto_label = getGraphviz ( test_graph ) instanzTrans "011"

instanzTrans :: ShowText knoten => GVTrans knoten
instanzTrans = GVTrans
	{ getGVProg = Default
	, getGVFormat = "png"
	, isGVDirected = True
	, getGVNID = showText -- Graphviz Knoten ID ist einfach show knoten
	, getGVNName = showText -- Knotenname auch
	, getGVNLabel = Nothing 
	, getGVNColor = Nothing
	, getGVNXAtts = Nothing
	, getGVELabel = Nothing
	, getGVEXAtts = Nothing
	}

-- start :: Graph a -> IO ()
-- start graph = do
start :: String -> IO ()
start file = do
	let 
   		-- fm      = listToFM $ zip [ 'a' .. 'f' ] [ 1 .. ]
			inFile  = file ++ ".dot"
			outFile = file ++ ".tmp"
			command = "dot" ++ " -o " ++ outFile ++ " " ++ inFile
	-- Problem: getGraphviz -> IO (String, GVFormat, ExitCode)
	-- getGraphviz ( graph ) instanzTrans "tmp"
	exitCode <- system command
	i <- readFile outFile
	putStr $ show $ zer2 i

-- und dann zu String konvertieren?
getPath :: IO ExitCode
getPath = system "pwd"

zer2 :: String -> [ GVKnoten ]
zer2 str = do
	ls <- lines str
	return $ il2 ls

il2 :: String -> GVKnoten
il2 str 
--	| str == ""           = parse_test_line -- [] -- parse_test_line
	| is_propper_line str = parse_line str -- ++ "\n" -- then parse line!!
	| isSpace $ head str  = il2 $ tail str
 	| otherwise           = emptyGVKnoten 

finite :: FiniteMap Char Char
finite = addToFM emptyFM 'a' 'b'

fin_2 :: FiniteMap Char Int
fin_2 = addToFM emptyFM 'a' 1

fin_3 :: FiniteMap GVKnoten Int
fin_3 = addToFM emptyFM emptyGVKnoten 1
-- instance of Ord GVKnoten required for definition of fin_3
-- require superclass: Eq GVKnoten

fin_4 :: FiniteMap GVKnoten Int
fin_4 = addToFM fin_3 emptyGVKnoten 2

fin_all :: [ GVKnoten ] -> FiniteMap GVKnoten Int -> FiniteMap GVKnoten Int
fin_all gv_node_list fm = listToFM $ zip (gv_node_list) [ 1 .. ]
--	| gv_node_list == [] = emptyFM
--	| otherwise          = addToFM 
-- listToFM $ zip ( knotenl g ) [ 1 .. ]

fin_5 :: (Ord a) => a -> FiniteMap a GVKnoten
fin_5 x = addToFM emptyFM x emptyGVKnoten

-- fin_9 :: (Show GVKnoten) => FiniteMap Int GVKnoten
-- fin_9 = addToFm (addToFM emptyFM emptyGVKnoten 1) emptyGVKnoten 2

{-fin_6 :: (Ord a) => [a] -> FiniteMap a GVKnoten
fin_6 lst@(x:xs)
	| lst == [] = emptyFM
	| otherwise = addToFM die_gegebene_fm -}

fin_7 :: (Ord a) => [a] -> [ GVKnoten ] -> FiniteMap a GVKnoten
fin_7 x_lst@(x:xs) gv_lst@(g:gs) 
	| gv_lst == [] = emptyFM
	|  x_lst == [] = emptyFM
	| otherwise    = listToFM $ zip x_lst gv_lst

fin_8 :: (Ord a) => [a] -> [ GVKnoten ] -> FiniteMap a GVKnoten
fin_8 x_lst gv_lst = listToFM $ zip x_lst gv_lst

out_1 :: String -> IO ()
out_1 = putStr.(++ "\n")

		out_2 :: String -> IO String
out_2 str = return ( str )

-- das gibt aber im hugs keinen output!! (zumindest keinen lesbaren)
out_3 :: String -> IO (String, Int)
out_3 str = return (str,17)

-- out_4 :: IO(String, Int) -> IO ()
-- out_4 (x,i) = putStrLn ("("++x++","++(show i)++")\n")

out_5 :: (Ord a, Enum a, Num a) => IO ( FiniteMap a GVKnoten )
out_5 = return (fin_8 [ 1 .. ] ptl_lst)

out_6 :: (Ord a, Enum a, Num a, ShowText a) => Graph a -> IO ( FiniteMap a GVKnoten )
out_6 graph = do
	-- rufe das graphviz auf und erstelle die .dot
	getGraphviz ( graph ) instanzTrans "0112"
	-- rufe oben auf und bearbeite
	return $ error "nicht fertig"

-- fin_x :: IO (FiniteMap GVKnoten Int)
-- fin_x = do
--	i <- readFile "011.tmp"
--	putStr $ show $ fin_all (zer2 i) emptyFM

-- showsFiniteMap :: FiniteMap a b -> [(a,b)] --ShowS
-- showsFiniteMap fm = (fmToList fm)

showFiniteMap :: (Show b, Show a) => FiniteMap a b -> String -- [(a,b)]
showFiniteMap fm = "listToFM" ++ show (fmToList fm)
-- ("("++) . (x ++ ",") 

showFM :: FiniteMap a b -> [(a,b)]
showFM fm = (fmToList fm)

-- show :: FiniteMap a GVKnoten -> String
-- show fm = show $ fmToList fm

-- showsFiniteMap :: FiniteMap a b -> ShowS
-- showsFiniteMap fm = ("asdf"++).(fmToList fm)

parse_test_line :: GVKnoten
parse_test_line = parse_line ptl1 -- ptl2

ptl_lst :: [ GVKnoten ]
ptl_lst = (parse_test_line):[parse_line ptl2]

emptyGVKnoten :: GVKnoten
emptyGVKnoten = GVKnoten { ident  = ""
							    , label  = "EMPTY NODE"
							    , pos    = (0,0)
							    , width  = 0
   							 , height = 0
							    } 

ptl1 :: String
ptl1 = "0 [label=\"test\", pos=\"172,170\", width=\"0.75\", height=\"0.7\"];"

ptl2 :: String
ptl2 = "0 [label=0, pos=\"172,170\", width=\"0.75\", height=\"0.7\"];"
	

parse_line :: String -> GVKnoten
parse_line str =
	GVKnoten { ident  = o12 str       -- :: String
			   , label  = o13 str       -- :: String
			   , pos    = o15 str       -- :: (Int, Int)
			   , width  = o20 str       -- :: Float
			   , height = o21 str       -- :: Float
			   }

o20 :: String -> Float
o20 str
	| ("width=\"" == take 7 str) = strToFloat $ float_only $ drop 7 str
	| otherwise                = o20 $ tail str 

o21 :: String -> Float
o21 str
	| ("height=\"" == take 8 str) = strToFloat $ float_only $ drop 8 str
	| otherwise                = o21 $ tail str 

strToFloat :: String -> Float
strToFloat string = ( read string ) :: Float

float_only :: String -> String
float_only str
	| str == []          = []
	| isDigit $ head str = [ head str ] ++ ( float_only $ tail str )
	| '.' == head str    = [ head str ] ++ ( digit_only $ tail str )
	| otherwise          = []

-- gibt ident_ zurück
o12 :: String -> String
o12 str 
	| isSpace $ head str = ""
	| otherwise          = [ head str ] ++ ( o12 $ tail str )

-- 13 und 14 geben label_ zurück
o13 :: String -> String 
o13 str
	| ("=\"" == take 2 str) = o14 $ drop 2 str
	| ('=' == head str)     = o14 $ tail str
	| otherwise             = o13 $ tail str

o14 :: String -> String
o14 str
	| ("\"," == take 2 str) = ""
	| (','   == head str  ) = ""
	| otherwise             = [ head str ] ++ ( o14 $ tail str )

o15 :: String -> ( Int , Int )
o15 str
	| ("pos=\"" == take 5 str) = ( help2
	, (read $ digit_only $ drop (6 + length(show help2)) str) )
	| otherwise                = o15 $ tail str 
	where 
			help2 = (read $ digit_only $ drop 5 str)::Int
			-- mit (read str)::Int = strToInt

digit_only :: String -> String
digit_only str
	| str == []          = []
	| isDigit $ head str = [ head str ] ++ ( digit_only $ tail str )
	| otherwise          = []

-- pattern:
-- number ++ " " ++ "[label"
is_propper_line :: String -> Bool
is_propper_line str 
	| str == ""                    = False
	| isDigit $ head str           = is_propper_line $ tail str
	| ( " [label=" == take 8 str ) = True
	| otherwise                    = False

info :: IO ()
info = do
	putStrLn "enter a.out + filename without .dot"

-------------------------------------------------------------------------------
{-
abzähl :: Ord a => Graph a -> Graph Integer
abzähl g =
    let fm  = listToFM $ zip ( knotenl g ) [ 1 .. ]
        f   = lookupWithDefaultFM fm (error "abzähl")
    in  mapG id f g
-}
