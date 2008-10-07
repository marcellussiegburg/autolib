module Autolib.Dot.Dotty where

--  $Id$


import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Hash

import qualified Autolib.Output as Output

import Autolib.Dot.Dot
import qualified Autolib.Dot.Graph
import Autolib.Size
import Autolib.Debug

import System.IO
import System.Directory
import qualified Control.Exception 

-- | write output as png to file,
-- in "current-directory\/..\/pics\/hashcode.{obj,dot,png}"
-- normally, current-directory = $HOME\/public_html\/cgi-bin
-- file name is built from hash value
-- if so named file is already there,
-- check whether it corresponds to the object
-- (by reading the *.obj file)
-- and if yes, re-use *.png (don't compute)
peng :: ( Hash a, Show a, ToDot a )
      => a
      -> Reporter ()
peng a = do
    let pics = "../pics"
        it = toDot a
        pre = pics ++ "/" ++ ( show $ abs $ hash a ) ++ "." ++ ( show $ toDotProgram a )
    let objfile = pre ++ ".obj"
        dotfile = pre ++ ".dot" 
        pngfile = pre ++ ".png"
    execute $ do
        flag <- doesDirectoryExist pics
        when ( not flag ) $ createDirectory pics
        done <- do
	       debug $ "looking for: " ++ pngfile
               ex <- doesFileExist pngfile
	       when (not ex) $ error "not"
	       debug $ "looking for: " ++ objfile
               cs <- readFile objfile
               debug $ "found      : " ++ objfile
	       let eq = show a == cs
	       debug $ "contents ok: " ++ show eq
               return $ eq
           `Control.Exception.catch` \ ( any :: Control.Exception.IOException ) -> return False
        when ( not done ) $ do      
             writeFileOver objfile $ show a
             writeFileOver dotfile $ show it ++ "\n\n"
             system' $ unwords 
		   [ progname $ toDotProgram a 
		   , toDotOptions a
		   , "-Tpng", "-o", pngfile
		   , dotfile
		   ]
             return ()
         `Control.Exception.catch` \ ( any :: Control.Exception.IOException ) -> return ()
    output $ Output.Image pngfile
    output $ Output.Beside
	        ( Output.Doc $ text 
			     $ "image rendered by " ++ show ( toDotProgram a )
		               ++ ", see " )
		( Output.Link $ "http://www.graphviz.org/" )

writeFileOver :: FilePath -> String -> IO ()
writeFileOver path cs = do
    ex <- doesFileExist path
    when ex $ removeFile path
    writeFile path cs

