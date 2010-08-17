-- -- $Id$

module Autolib.Graph.Gallery where

import Autolib.Graph.Type
import Autolib.Graph.Basic
import Autolib.Graph.Ops

import Autolib.Graph.Display
import Autolib.Dot.Dot
import Autolib.Util.Zufall

-- | schreibt jeden graph in ein eps-file
-- schreibt ein tex-file, daß alle diese included
-- und ein tex-file mit allen texinfos (gewürfelt)
mkQuiz :: GraphC Int
       => FilePath
       -> [ Graph Int ]
       -> IO ()
mkQuiz  prefix gs = do
    putStrLn $ "Quiz mode"
    gs' <- permutation gs
    pics <- mapM ( \ (i,g) -> mkPic (prefix ++ "_" ++ show i) g )
	  $ zip [0..] gs'

    let fname = prefix ++ ".tex"
    putStrLn $ "writing tex file " ++ fname
    writeFile fname $ unlines $ do
        p <- pics
	line <- [ "\\begin{minipage}{4cm}"
		, "\\includegraphics[width=4cm,height=4cm]{" ++ p ++ "}" 
		, "\\end{minipage}"
		, "\\quad"
		]
	return line

    let fname = prefix ++ "_map.tex"
    putStrLn $ "writing map file " ++ fname
    is <- permutation $ map texinfo gs
    writeFile fname $ unlines $ do
        i <- is
	return $ "\\(" ++ i ++ "\\),~ "

    return ()




-- | schreibt jeweils in benanntes epsfile
mkGallery :: GraphC Int
	  => [ (FilePath, Graph Int) ]
	  -> IO ()
mkGallery fgs = do
    putStrLn $ "Gallery mode"
    mapM_ (uncurry mkPic) fgs


mkPic :: ( GraphC a )
      => FilePath -> Graph a
      -> IO FilePath
mkPic fname g = do
    putStrLn $ "writing eps file " ++ fname
    meps fname g

