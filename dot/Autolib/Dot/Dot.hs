module Dot.Dot where

import qualified Dot.Graph
import System
import Monad ( when )

class ToDot a where 
      toDot :: a -> Dot.Graph.Type



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
	  return ( fmtfile , fmt , ex )
