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
--	     { getGVProg :: GVProg
--       , getGVFormat :: GVFormat
--       , isGVDirected :: Bool
--       , getGVNID :: a -> GVNodeID
--	     , getGVNName :: a -> GVName
--       , getGVNLabel :: Maybe (a -> GVLabel)
--       , getGVNColor :: Maybe (a -> GVColor)
--       , getGVNXAtts :: Maybe (a -> GVXAtts)
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
--     getGVProg   - ist entweder Default, Dot oder Neato
--                 - beschreibt welches Graphviz Tool genutzt werden soll
--                 - Empfehlungen kann ich Zur Zeit noch nicht geben
--     getGVFormat - Format in dem die Ausgabe sein soll.
--                 - Möglich sind alle Formate, die Graphviz unterstützt
--                 - Beispiele: "ps" "gif" "png"
--                 - Bitte achte auf gültige Angaben.
--     isGVDirected - True, falls der Graph gerichtet ist; False, falls nicht
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
--     getGVELabel - Das Label der Kante.
--     getGVEXAtts - Beliebige Graphviz Attribute der Kante.
--                   Siehe Graphviz Docu.
--
--   Einfaches Beispiel:
--
--   myTrans :: Show a => GVTrans a  -- Typangabe, falls Knotentyp frei
--   myTrans = GVTrans
--	     { getGVProg :: Default
--       , getGVFormat :: "gif"
--       , isGVDirected :: False
--       , getGVNID :: show
--	     , getGVNName :: show
--       , getGVNLabel :: Nothing
--       , getGVNColor :: Nothing
--       , getGVNXAtts :: Nothing
--       , getGVELabel :: Nothing
--       , getGVEXAtts :: Nothing
--       }
--
--   Beispiel, wenn man Informationen hineinbringen will:
--
--   myTrans :: (Show knoten, Ord knoten)
--       => (Labeling knoten) -> (GVTrans knoten)
--   myTrans labeling = GVTrans
--	     { getGVProg :: Default
--       , getGVFormat :: "gif"
--       , isGVDirected :: False
--	     , getGVNID = show
--	     , getGVNName = show
--	     , getGVNLabel = Just (getNLabel labeling)
--	     , getGVNColor = Nothing
--	     , getGVNXAtts = Nothing
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
-- Autor: Alexander Kiel (mai99bxd@studserv.uni-leipzig.de)
-- Version: 28.05.2002 - 1
--------------------------------------------------------------------------------


module Graph.Viz
	( GVTrans (..)
	, GVName, GVLabel, GVColor, GVXAtts
	, GVProg (..), GVFormat
	, getGraphviz
	, ExitCode
	, Graphviz   -- braucht man eigentlich nicht außerhalb
	, transGtoGV -- braucht man eigentlich nicht außerhalb
	, ShowText, showText
) where

import Graph.Graph
import FiniteMap
import Sets
import ToDoc -- wegen Show Set und Show FiniteMap - FIX!
import System

import Util.Datei (perm)

import Char (showLitChar)

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
data GVProg = Default | Dot | Neato
type GVFormat = String

-- Datenstruktur des Graphviz
data Graphviz = Graphviz
	{ nodesGV :: GVNodeMap
	, edgesGV :: Set GVEdge
	, directedGV :: Bool
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
	, labelGVE :: Maybe GVLabel
	, xattsGVE :: Maybe GVXAtts
	} deriving (Show, Eq, Ord)

instance ToDoc GVEdge where toDoc = text . show
	
instance Show GVProg where
	show Default = "dot" 
	show Dot = "dot"
	show Neato = "neato"

-- Datenstruktur der Transformationsmatrix
data GVTrans a = GVTrans
	{ getGVProg :: GVProg
	, getGVFormat :: GVFormat
	, isGVDirected :: Bool
	, getGVNID :: a -> GVNodeID
	, getGVNName :: a -> GVName
	, getGVNLabel :: Maybe (a -> GVLabel)
	, getGVNColor :: Maybe (a -> GVColor)
	, getGVNXAtts :: Maybe (a -> GVXAtts)
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
	, directedGV = (isGVDirected trans)
	}

-- die Eingabedatei für das Graphviz Tool erzeugen
makeGVIntput :: Graphviz -> String
makeGVIntput (Graphviz nodes edges directed) =
	showsGV directed "G" (fmToList nodes) (setToList edges) ""
	
-- Graphviz erstellen und den Dateinamen der Graphviz Datei zurückgeben
-- Graph -> GVTrans -> vollständiger Path mit Dateiname ohne Endung ->
-- voller Path mit vollem Dateinamen (Endung wird angehangen)
getGraphviz :: (Graph a) -> (GVTrans a) -> String ->
	IO (String, GVFormat, ExitCode)
getGraphviz graph trans path = do
	let
		inFile0 = path ++ ".dot"
		outFile = path ++ "." ++ (getGVFormat trans)

		-- das passiert, falls getGVFormat == "dot"
                inFile = if inFile0 == outFile
			 then -- neuen infilenamen erfinden
			      path ++ ".tmp"
			 else inFile0

		command = show (getGVProg trans) ++ " -T" ++ (getGVFormat trans) ++
			" -o " ++ outFile ++ " " ++ inFile
			
	writeFile inFile (makeGVIntput $ transGtoGV graph trans)		
	-- putStrLn $ "running command: " ++ show command

	exitCode <- system command	
	perm "go+r" outFile
	
	return (outFile, (getGVFormat trans), exitCode)
	
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
transGEtoGVE :: (GVTrans a) -> (Kante a) -> GVEdge
transGEtoGVE trans edge = GVEdge
	{ idGVN1 = (getGVNID trans) (von edge)
	, idGVN2 = (getGVNID trans) (nach edge)
	, labelGVE = doMaybe (getGVELabel trans) edge
	, xattsGVE = doMaybe (getGVEXAtts trans) edge
	}

-------------------------------------------------------------------------------
-- hier folgen Funktionen zum Erzeugen der Eingabedatei für das Graphviz Tool
-------------------------------------------------------------------------------

showsGV :: Bool -> String -> [(GVNodeID, GVNode)] -> [GVEdge] -> ShowS
showsGV directed graphID nodes edges =
	showsGVHeader directed graphID .
	showsNodeDefs nodes .
	("\n" ++) .
	showsEdges edges directed .
	("}" ++)
	

-- showsGraphHeader (graphID)
showsGVHeader :: Bool -> String -> ShowS
showsGVHeader directed graphID =
	showsGraphKeyword directed . shows graphID . ("{\n" ++)
	where
	showsGraphKeyword True = ("digraph " ++)
	showsGraphKeyword False = ("graph " ++)

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
		showsColor (Just nodeColor) = (",style=filled,color=" ++) . shows nodeColor

showsEdges :: [GVEdge] -> Bool -> ShowS
showsEdges (edge:rest) directed =
	showsEdge (idGVN1 edge) (idGVN2 edge) directed (labelGVE edge)
	(xattsGVE edge) . showsEdges rest directed
showsEdges [] _ = ("" ++)

-- showsEdge (node1ID, node2ID, directed, edgeLabel)
showsEdge :: GVNodeID -> GVNodeID -> Bool -> Maybe GVLabel -> Maybe GVXAtts ->
	ShowS
showsEdge node1ID node2ID directed edgeLabel edgeXAtts =
	("\t" ++) . (node1ID ++) . (' ':) . (showsArrow directed) .
	(' ':) . (node2ID ++) . (showsAtts edgeLabel edgeXAtts) . (";\n" ++)
	where
		showsArrow :: Bool -> ShowS
		showsArrow False = ("--" ++)
		showsArrow True = ("->" ++)
		
		showsAtts :: Maybe GVLabel -> Maybe GVXAtts -> ShowS
		showsAtts Nothing Nothing = ("" ++)
		showsAtts (Just edgeLabel) Nothing =
			(" [label=" ++) . shows edgeLabel . (']':)
		showsAtts Nothing (Just edgeXAtts) =
			("[label=\"\"" ++) . showsXAtts edgeXAtts . (']':)
		showsAtts (Just edgeLabel) (Just edgeXAtts) =
			(" [label=" ++) . shows edgeLabel . showsXAtts edgeXAtts . (']':)
		
		showsXAtts :: GVXAtts -> ShowS
		showsXAtts (xAttr:xAtts) =
			(", " ++) . (fst xAttr ++) . ("=\"" ++) . (snd xAttr ++) . ("\"" ++) .
			showsXAtts xAtts
		showsXAtts [] = ("" ++) 

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

-------------------------------------------------------------------------------
-- hier folgt die Klasse ShowText
-------------------------------------------------------------------------------

-- Die Klasse ShowText macht fast das gleiche wie die Klasse Show.
-- Der einzige Unterschied besteht darin, dass eingebettete Strings oder
-- Chars nicht gequotet dargestellt werden, sondern die Zeichen " und '
-- entfernt werden.
-- ACHTUNG: showText ist keine eineindeutige Abbildung von a -> String
--          showText (1, 2) == showText ("1", 2) == showText ('1', 2) ...

class ShowText a where
	showText :: a -> String
	
instance ShowText Char where
	showText c = showLitChar c ""

instance ShowText String where
	showText cs = shows cs ""
		where
			shows :: String -> ShowS
			shows "" = ("" ++)
			shows ('"':cs) = shows cs
			shows ('\'':cs) = shows cs
			shows (c:cs) = showLitChar c . shows cs		
		
instance Show a => ShowText	a where
	showText x = showText $ show x
		
		
