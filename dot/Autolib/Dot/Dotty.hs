module Autolib.Dot.Dotty where

--  $Id$


import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Hash

import qualified Autolib.Output as Output

import Autolib.Dot.Dot
import qualified Autolib.Dot.Graph
import Autolib.Size

import System.IO
import Control.Exception ( catch )

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
    let it = toDot a
        pre = "../pics/" ++ ( show $ abs $ hash a )
    let objfile = pre ++ ".obj"
        dotfile = pre ++ ".dot" 
        pngfile = pre ++ ".png"
    execute $ do
        done <- do
           cs <- readFile objfile
           return $ show a == cs
         `Control.Exception.catch` \ any -> return False
        when ( not done ) $ do      
             writeFile objfile $ show a
             writeFile dotfile $ show it ++ "\n\n"
             system' $ unwords 
		   [ progname $ toDotProgram a 
		   , toDotOptions a
		   , "-Tpng", "-o", pngfile
		   , dotfile
		   ]
             return ()
         `Control.Exception.catch` \ any -> return ()
    output $ Output.Image pngfile

