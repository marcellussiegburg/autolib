--------------------------------------------------------------------------------
--
-- Modul, welches einen Graph mittels Graphviz darstellen kann
--
-- Die Erstellung der Ausgabedatei mit dem Graphen ist dreistufig
--   1. Transformation Graph -> Graphviz mittels GVTrans
--   2. erzeugen der Eingabedatei für das Graphviztool
--   3. Aufruf des Graphviztools welches die Ausgabedatei erzeugt
--
-- Der Graphviz - Graph:
--   - ist eine Datenstruktur eines Graphen, der alle für die Darstellung
--     benötigten Informationen enthält
--   - im Gegensatz dazu enthält ein normaler Graph keine Informationen zur
--     Darstellung
--   - die Informationen werden aus der Transformationsdatenstruktur GVTrans
--     entnommen   
--   - wenn man lediglich seinen Graphen darstellen will, braucht einen der
--     Graphviz nicht zu interessieren - viel wichtiger ist GVTrans
--
-- GVTrans:
--
--   data GVTrans a = GVTrans
--	     { getGVNID :: a -> GVNodeID
--	     , getGVNName :: a -> GVName
--       , getGVNLabel :: Maybe (a -> GVLabel)
--       , getGVNColor :: Maybe (a -> GVColor)
--       , getGVNXAtts :: Maybe (a -> GVXAtts)
--       , isGVEDirected :: Kante a -> Bool
--       , getGVELabel :: Maybe (Kante a -> GVLabel)
--       , getGVEXAtts :: Maybe (Kante a -> GVXAtts)
--       }
--
--   - alle Felder sind Funktionen oder Maybe Funktionen
--     falls man eine optionale Angabe nicht machen will schreibt man einfach
--     "Nothing" hin
--   - Die Funktionen muss man selbst schreiben. Sie bilden entweder von einem
--     Knoten oder einer Kante auf einen Wert ab.
--
--   Einzelne Funktionen:
--
--     getGVNID    - soll eine möglichst kurze eindeutige ID eines Knotens
--                   zurückgeben
--                 - idealerweise "Kx" wobei x eine laufende Nummer ist
--                 - diese ID wird nur intern verwendet
--                 - Ich wollte die eigentlich selber erzeugen. Nur wird es
--                   ziemlich komplex, wenn ich die Knoten erst durchnummeriere
--                   und dann bei jeder Kante schauen muss, welche ID ich dem
--                   Knoten gegeben habe. Ich werd das später vieleicht mal
--                   implementieren und die externe ID nicht mehr nutzen.
--                 - für den Anfang reicht übrigends ein "show", falls der
--                   Knoteninhalt nicht zu groß ist.
--     getGVNName  - Der Name des Knotens, der neben dem möglichen Label im
--                   Knoten steht. Es sind "\n"'s erlaubt.
--                 - Normalerweise sollte hier nur "show" stehen.
--     getGVNLabel - Optional, sonst wie Name, steht auch im Knoten
--                 - Wurde aus Gründen der möglicherweise anderen Darstellung
--                   extra gemacht.
--     getGVNColor - Hintergrundfarbe des Knotens. Hier sind übliche englische
--                   Farbbezeichnungen oder was das hier "#%2x%2x%2x" RGB auch
--                   immer heißen man. FIX!
--                   http://www.research.att.com/~erg/graphviz/info/colors.html
--     getGVNXAtts - Beliebige Graphviz Attribute des Knotens.
--                   Siehe Graphviz Docu.
--     getGVEDirected - Soll zurückgeben, ob die Kante gerichtet, oder nicht
--                      gerichtet ist.
--                    - Für die meißten sollte ein einfaches "(\x -> False)"
--                      reichen.
--     getGVELabel - Das Label der Kante.
--     getGVEXAtts - Beliebige Graphviz Attribute der Kante.
--                   Siehe Graphviz Docu.
--
--   Einfaches Beispiel:
--
--   myTrans :: Show a => GVTrans a  -- Typangabe, falls Knotentyp frei
--   myTrans = GVTrans
--	     { getGVNID :: show
--	     , getGVNName :: show
--       , getGVNLabel :: Nothing
--       , getGVNColor :: Nothing
--       , getGVNXAtts :: Nothing
--       , isGVEDirected :: (\x -> False)
--       , getGVELabel :: Nothing
--       , getGVEXAtts :: Nothing
--       }
--
--   Beispiel, wenn man Informationen hineinbringen will:
--
--   myTrans :: (Show knoten, Ord knoten)
--       => (Labeling knoten) -> (GVTrans knoten)
--   myTrans labeling = GVTrans
--	     { getGVNID = show
--	     , getGVNName = show
--	     , getGVNLabel = Just (getNLabel labeling)
--	     , getGVNColor = Nothing
--	     , getGVNXAtts = Nothing
--	     , isGVEDirected = (\x -> False)
--	     , getGVELabel = Just (getELabel labeling)
--	     , getGVEXAtts = Nothing
--	     }
--
--   getNLabel und getELabel müssen natürlich noch implementiert werden.
--
-- Namenskonventionen:
--   - ich verwende englische Bezeichner
--   - der Typ eines Graphen, welcher die für Graphviz benötigten Informationen
--     enthält heißt "Graphviz"
--   - "Graphviz" wird an anderen Stellen mit "GV" abgekürzt
--     "GV" ist auch gleichzeitig der Namespace für alle Funktionen und Typen,
--     um in Haskell nicht anzustoßen
--   - die Transformationsstruktur mit der man einen Graphen in einen Graphviz
--     umwandeln kann heißt "GVTrans"
--   - da in Haskell Bezeichner außer Typen nicht mit einem Großbuchstaben
--     anfangen dürfen, heißt der Name eines Knotens nicht "GVNName", sondern
--     "nameGVN", wobei "GVN" ein Grahpviz Node ist
--   - "GVXAtts" sind Graphviz eXtended Attributes also eine Liste von
--     "GVXAttr"'s
--
-- Autor: Alexander Kiel
-- Version: 24.05.2002
--------------------------------------------------------------------------------


module Graph.Viz
	( Graphviz -- braucht man eigentlich nicht außerhalb
	, GVTrans
		( getGVNID
		, getGVNName
		, getGVNLabel
		, getGVNColor
		, getGVNXAtts
		, isGVEDirected
		, getGVELabel
		, getGVEXAtts
		)
	, GVName, GVLabel, GVColor, GVXAtts
	, transGtoGV -- braucht man eigentlich nicht außerhalb
	, getGraphviz
	)
	where

import Graph.Graph
import FiniteMap
import Set
import System

-------------------------------------------------------------------------------
-- hier folgen Datenstrukturen
-------------------------------------------------------------------------------

-- Datenstruktur des Graph
{-
data Graph a  = Graph
	{ knoten    :: Set a
	, kanten    :: Set (Kante a)
	} deriving (Read)
-}

-- einige Typen
type GVNodeID = String
type GVName = String
type GVLabel = String
type GVColor = String
type GVXAttr = (String, String)
type GVXAtts = [GVXAttr]
type GVNodeMap = FiniteMap GVNodeID GVNode

-- Datenstruktur des Graphviz
data Graphviz = Graphviz
	{ nodesGV :: GVNodeMap
	, edgesGV :: Set GVEdge
	} deriving (Show)
	
data GVNode = GVNode
	{ nameGVN :: GVName
	, labelGVN :: Maybe GVLabel
	, colorGVN :: Maybe GVColor
	, xattsGVN :: Maybe GVXAtts
	} deriving (Show)

data GVEdge = GVEdge
	{ idGVN1 :: GVNodeID
	, idGVN2 :: GVNodeID
	, directedGVE :: Bool
	, labelGVE :: Maybe GVLabel
	, xattsGVE :: Maybe GVXAtts
	} deriving (Show, Eq, Ord)

instance Show (FiniteMap GVNodeID GVNode) where
	show fmap = "listToFM" ++ show (fmToList fmap)

-- Datenstruktur der Transformationsmatrix
data GVTrans a = GVTrans
	{ getGVNID :: a -> GVNodeID
	, getGVNName :: a -> GVName
	, getGVNLabel :: Maybe (a -> GVLabel)
	, getGVNColor :: Maybe (a -> GVColor)
	, getGVNXAtts :: Maybe (a -> GVXAtts)
	, isGVEDirected :: Kante a -> Bool
	, getGVELabel :: Maybe (Kante a -> GVLabel)
	, getGVEXAtts :: Maybe (Kante a -> GVXAtts)
	}

-------------------------------------------------------------------------------
-- hier folgen die drei Hauptfunktionen
-------------------------------------------------------------------------------

-- einen Graph in einen Graphviz transformieren
transGtoGV :: (Graph a) -> (GVTrans a) -> Graphviz
transGtoGV (Graph nodes edges) trans = Graphviz
	{ nodesGV = makeGVNodes (setToList nodes) trans
	, edgesGV = mapSet (transGEtoGVE trans) edges
	}

-- die Eingabedatei für das Graphviz Tool erzeugen
makeGVIntput :: Graphviz -> String
makeGVIntput (Graphviz nodes edges) =
	showsGV "G" (fmToList nodes) (setToList edges) ""
	
-- Graphviz erstellen und den Dateinamen der Graphviz Datei zurückgeben
-- Graph -> GVTrans -> vollständiger Path mit Dateiname ohne Endung ->
-- voller Path mit vollem Dateinamen (Endung wird angehangen)
getGraphviz :: (Graph a) -> (GVTrans a) -> String -> String ->
	IO (String, String, ExitCode)
getGraphviz graph trans path typ = do
	let
		outFile = path ++ ".dot"
		typFile = path ++ "." ++ typ
	writeFile outFile (makeGVIntput $ transGtoGV graph trans)
	let
		command = 
			"cat " ++ outFile ++ " | dot -T" ++ typ ++ " -o " ++ typFile
			
	exitCode <- system command	
	
	return (typFile, command, exitCode)
	
-------------------------------------------------------------------------------
-- hier folgen Funktionen der Transformation
-------------------------------------------------------------------------------

-- alle Graphviz Knoten erstellen
makeGVNodes :: [a] -> (GVTrans a) -> FiniteMap GVNodeID GVNode
makeGVNodes nodeList trans = foldl add emptyFM (zip nodeList [1..])
	where
	-- nodeID wird zur Zeit nicht verwendet, sondern die externe von trans
	add nodeFM (node, nodeID) =
		addToFM nodeFM ((getGVNID trans) node)
		( transGNtoGVN
		  (getGVNName trans) (getGVNLabel trans)
		  (getGVNColor trans) (getGVNXAtts trans)
		  node
		)

-- einen Knoten eines Graphen in einen Knoten eines Graphviz transformieren
transGNtoGVN :: (a -> GVName) -> Maybe (a -> GVLabel) ->
	Maybe (a -> GVColor) -> Maybe (a -> GVXAtts) -> a -> GVNode
transGNtoGVN getGVNName getGVNLabel getGVNColor getGVNXAtts node = GVNode
	{ nameGVN = getGVNName node
	, labelGVN = doMaybe getGVNLabel node
	, colorGVN = doMaybe getGVNColor node
	, xattsGVN = doMaybe getGVNXAtts node
	}

-- eine Kante eines Graphen in eine Kante eines Graphviz transformieren
transGEtoGVE :: Ord GVEdge => (GVTrans a) -> (Kante a) -> GVEdge
transGEtoGVE trans edge = GVEdge
	{ idGVN1 = (getGVNID trans) (von edge)
	, idGVN2 = (getGVNID trans) (nach edge)
	, directedGVE = (isGVEDirected trans) edge
	, labelGVE = doMaybe (getGVELabel trans) edge
	, xattsGVE = doMaybe (getGVEXAtts trans) edge
	}

-------------------------------------------------------------------------------
-- hier folgen Funktionen zum Erzeugen der Eingabedatei für das Graphviz Tool
-------------------------------------------------------------------------------

showsGV :: String -> [(GVNodeID, GVNode)] -> [GVEdge] -> ShowS
showsGV graphID nodes edges =
	showsGVHeader graphID .
	showsNodeDefs nodes .
	("\n" ++) .
	showsEdges edges .
	("}" ++)
	

-- showsGraphHeader (graphID)
showsGVHeader :: String -> ShowS
showsGVHeader graphID = ("graph " ++) . shows graphID . ("{\n" ++)

showsNodeDefs :: [(GVNodeID, GVNode)] -> ShowS
showsNodeDefs ((nodeID, node):rest) =
	showsNodeDef nodeID (nameGVN node) (labelGVN node) (colorGVN node) .
	showsNodeDefs rest
showsNodeDefs _ = ("" ++)

-- showsNodeDef (nodeID, nodeName, nodeLabel, nodeColor)
showsNodeDef :: GVNodeID -> GVName -> Maybe GVLabel -> Maybe GVColor -> ShowS
showsNodeDef nodeID nodeName nodeLabel nodeColor =
	("\t" ++) . (nodeID ++) . (" [label=" ++) .
	(showsNameAndLabel nodeName nodeLabel) .
	(showsColor nodeColor) .
	("];\n" ++)
	where
		showsNameAndLabel nodeName Nothing = shows nodeName
		showsNameAndLabel nodeName (Just nodeLabel) =
			shows (nodeName ++ "(" ++ nodeLabel ++ ")")
		showsColor Nothing = ("" ++)
		showsColor (Just nodeColor) = (", color=" ++) . shows nodeColor

showsEdges :: [GVEdge] -> ShowS
showsEdges (edge:rest) =
	showsEdge (idGVN1 edge) (idGVN2 edge) (directedGVE edge) (labelGVE edge) .
	showsEdges rest
showsEdges _ = ("" ++)

-- showsEdge (node1ID, node2ID, directed, edgeLabel)
showsEdge :: GVNodeID -> GVNodeID -> Bool -> Maybe GVLabel -> ShowS
showsEdge node1ID node2ID directed edgeLabel =
	("\t" ++) . (node1ID ++) . (' ':) . (showsArrow directed) .
	(' ':) . (node2ID ++) . (showsLabel edgeLabel) . (";\n" ++)
	where
		showsArrow False = ("--" ++)
		showsArrow True = ("->" ++)
		showsLabel Nothing = ("" ++)
		showsLabel (Just edgeLabel) = (" [label=" ++) . shows edgeLabel . (']':)

-------------------------------------------------------------------------------
-- hier folgen Hilfsfunktionen
-------------------------------------------------------------------------------

doMaybe :: Maybe (a -> b) -> a -> Maybe b
doMaybe Nothing _ = Nothing
doMaybe (Just f) a = Just (f a)

-- macht aus der Integer NodeID eine mit führenden Buchstaben, weil das
-- Graphviz so will
-- wird nicht mehr gebraucht
makeNodeID :: Int -> String
makeNodeID i = 'K' : show i

