#! /usr/bin/env runghc

> module Main (main) where

> import Distribution.Simple
> import Distribution.Simple.LocalBuildInfo
> import Distribution.Simple.BuildPaths

> import Control.Monad
> import System.FilePath
> import System.Directory
> import Data.List

> main :: IO ()
> main = defaultMainWithHooks simpleUserHooks{ postConf = pcHook }

> progs = ["dot", "neato", "twopi", "circo", "fdp"]

> pcHook args config pkg lbi = do
>     paths <- mapM findExecutable progs
>
>     let file = autogenModulesDir lbi </> "Autolib" </> "Local.hs"
>         (dir, _) = splitFileName file
>         missing = [prog | (prog, Nothing) <- zip progs paths]
>
>     when (not . null $ missing) $ do
>         putStrLn $ "*** Warning: Could not find some graphviz executables: "
>             ++ concat (intersperse ", " missing) ++ "\nPlease install "
>             ++ "graphviz and make sure the executables are on your PATH."
>
>     createDirectoryIfMissing True dir
>     writeFile file $ contents paths

> contents paths =
>     "module Autolib.Local where\n" ++
>     "\n" ++
>     do  (prog, path) <- zip progs paths
>         (prog ++ " :: FilePath\n" ++
>          prog ++ " = " ++ show (maybe prog id path) ++ "\n\n")
