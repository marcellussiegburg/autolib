module Dot.Dot where

import qualified Dot.Graph
import System
import Monad ( when )
import Random

import Util.Datei ( perm )

class ToDot a where 
      toDot :: a -> Dot.Graph.Type
      toDotProgram :: a -> String
      toDotProgram a = "dot" -- default für gerichtete graphen

instance ToDot a => ToDot [a] where
    toDotProgram xs = toDotProgram ( head xs )
    toDot xs = Dot.Graph.besides $ map toDot xs


-- gleicher argument/resultat-typ wie Graph.Viz.getGraphviz

mkDot :: ToDot a 
      => a -> String -> String -> FilePath 
      -> IO ( FilePath, String, ExitCode )
mkDot a prog fmt path = do

    let dotfile = path ++ ".dot"
    writeFile dotfile $ show $ toDot a

    let fmtfile = path ++ "." ++ fmt

    if    ( not $ prog `elem` [ "dot", "neato" ]  )
       || ( not $ fmt  `elem` [ "png", "ps"    ]  )
       then do   
          return ( fmtfile, fmt, ExitFailure 1 )
       else do
	  ex <- system $ unwords [ prog, "-T" ++ fmt, "-o", fmtfile, dotfile ]
	  perm "go+r" fmtfile
	  return ( fmtfile , fmt , ex )

--------------------------------------------------------------------------

display :: ToDot a => a -> IO ()
display a = do
    n <- randomRIO (0,10000 :: Int)
    let fname = "/tmp/display." ++ show n
    let dotfile = fname ++ ".dot"
    let epsfile = fname ++ ".eps"
    writeFile dotfile $ show $ toDot $ a
    let p = toDotProgram a
    system $ unwords 
	   [ p , "-Tps" , "-Grankdir=LR", dotfile , "-o", epsfile ]
    system $ unwords [ "rm" , dotfile ]
    system $ unwords
	   [ "gv" , epsfile ]
    system $ unwords [ "rm" , epsfile ]
    return ()
