module Graph.Gallery where

-- $Id$

import Graph.Type
import Graph.Basic
import Graph.Ops

import Graph.Display
import Dot.Dot

mkGallery :: FilePath 
	  -> [ Graph Int ]
	  -> IO ()
mkGallery prefix gs = do
    pics <- mapM (mkPic prefix) $ zip [0..] gs

    let fname = prefix ++ ".tex"
    putStrLn $ "writing tex file " ++ fname
    writeFile fname $ unlines $ do
        (p, g) <- zip pics gs
	line <- [ "\\begin{minipage}{4cm}"
		, "\\includegraphics[width=4cm,height=4cm]{" ++ p ++ "}" 
		, "\\verb|" ++ show (info g) ++ "|"
		, "\\end{minipage}"
		, "\\quad"
		]
	return line
    return ()

mkPic :: ( Ord a , Show a )
      => FilePath -> (Int, Graph a) 
      -> IO FilePath
mkPic prefix (i, g) = do
    let fname = prefix ++ "_" ++ show i
    putStrLn $ "writing eps file " ++ fname
    meps fname g

