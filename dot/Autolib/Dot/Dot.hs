module Dot.Dot where

import qualified Dot.Graph
import System
import Monad ( when )

class ToDot a where 
      toDot :: a -> Dot.Graph.Type



-- gleicher argument/resultat-typ wie Graph.Viz.getGraphviz

mkDot :: ToDot a 
      => a -> String -> FilePath 
      -> IO ( FilePath, String, ExitCode )
mkDot a prog path = do

    let dotfile = path ++ ".dot"
    writeFile dotfile $ show $ toDot a

    let fmt = "png"
    let fmtfile = path ++ "." ++ fmt

    if not $ prog `elem` [ "dot", "neato" ] 
       then do   
          return ( fmtfile, fmt, ExitFailure 1 )
       else do
	  ex <- system $ unwords [ prog, "-T" ++ fmt, "-o", fmtfile, dotfile ]
	  return ( fmtfile , fmt , ex )
