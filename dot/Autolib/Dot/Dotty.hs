{-# language ScopedTypeVariables #-}

module Autolib.Dot.Dotty where

--  $Id$


import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Hash

import qualified Autolib.Output as Output

import Autolib.Dot.Dot
import qualified Autolib.Dot.Graph
import Autolib.Size

import qualified System.IO.Strict as SIO
import System.Directory
import qualified Control.Exception as CE

import qualified Data.ByteString as B

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
        action = do
            flag <- doesDirectoryExist pics
            when ( not flag ) $ createDirectory pics
            done <- do
                let debug msg = return ()
                debug $ "looking for: " ++ pngfile
                ex <- doesFileExist pngfile
                when (not ex) $ ioError $ userError "not"
                debug $ "looking for: " ++ objfile
                cs <- SIO.run $ SIO.readFile objfile
                debug $ "found      : " ++ objfile
                let eq = show a == cs
                debug $ "contents ok: " ++ show eq
                return $ eq
             `CE.catch` \ ( any :: CE.IOException ) -> return False
            when ( not done ) $ do
                 writeFileOver objfile $ show a
                 writeFileOver dotfile $ show ( toDoc it ) ++ "\n\n"
                 system' $ unwords
                       [ progname $ toDotProgram a
                       , toDotOptions a
                       , "-Tpng", "-o", pngfile
                       , dotfile
                       ]
                 return ()
             `CE.catch` \ ( any :: CE.IOException ) -> return ()
    -- execute action -- WHAT?
    output $ Output.HRef pngfile
           $ Output.Image pngfile (action >> B.readFile pngfile)

writeFileOver :: FilePath -> String -> IO ()
writeFileOver path cs = do
    ex <- doesFileExist path
    when ex $ removeFile path
    writeFile path cs

