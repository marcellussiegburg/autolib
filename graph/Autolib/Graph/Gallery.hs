module Graph.Gallery where

-- -- $Id$

import Graph.Type
import Graph.Basic
import Graph.Ops

import Graph.Display
import Dot.Dot
import Util.Zufall

mkQuiz :: FilePath
       -> [ Graph Int ]
       -> IO ()
-- schreibt jeden graph in ein eps-file
-- schreibt ein tex-file, daß alle diese included
-- und ein tex-file mit allen texinfos (gewürfelt)
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




mkGallery :: [ (FilePath, Graph Int) ]
	  -> IO ()
-- schreibt jeweils in benanntes epsfile
mkGallery fgs = do
    putStrLn $ "Gallery mode"
    mapM_ (uncurry mkPic) fgs


mkPic :: ( Ord a , Show a )
      => FilePath -> Graph a
      -> IO FilePath
mkPic fname g = do
    putStrLn $ "writing eps file " ++ fname
    meps fname g

