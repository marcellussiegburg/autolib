module Autolib.Dot.Dot 

( ToDot (..)
, Layout_Program (..)
, display
, system'
, progname
, mot, meps
)

where

--  $Id$

import qualified Autolib.Dot.Graph
import Control.Monad ( when )
import System.Random
import System.Exit
import System.Cmd
import System.IO

import Autolib.Util.Chmod

import Autolib.Local
import Autolib.Reader
import Autolib.ToDoc
import Autolib.Debug

data Layout_Program = Dot
		    | Neato
		    | Twopi
		    | Circo
		    | Fdp
     deriving ( Eq, Ord, Read, Show )

progname :: Layout_Program -> String
progname p = case p of
	 Dot   -> Autolib.Local.dot
	 Neato -> Autolib.Local.neato
	 Twopi -> Autolib.Local.twopi
	 Circo -> Autolib.Local.circo
	 Fdp   -> Autolib.Local.fdp

class ToDot a where 
      toDot :: a -> Autolib.Dot.Graph.Type
      toDotProgram :: a -> Layout_Program
      -- default für gerichtete graphen:
      toDotProgram a = Dot
      toDotOptions :: a -> String
      toDotOptions a = "-Grankdir=LR"

instance ToDot a => ToDot [a] where
    toDotProgram xs = toDotProgram ( head xs )
    toDot xs = Autolib.Dot.Graph.besides $ map toDot xs

-- gleicher argument/resultat-typ wie Graph.Viz.getGraphviz

mkDot :: ToDot a 
      => a -> Layout_Program -> String -> FilePath 
      -> IO ( FilePath, String, ExitCode )
mkDot a prog fmt path = do

    let dotfile = path ++ ".dot"
    writeFile dotfile $ show $ toDot a

    let fmtfile = path ++ "." ++ fmt

    if ( not $ fmt  `elem` [ "png", "ps"    ]  )
       then do   
          error $ unwords [ "illegal format:", fmt ]
          return ( fmtfile, fmt, ExitFailure 1 )
       else do
	  ex <- system' $ unwords 
		[ progname prog, "-T" ++ fmt, "-o", fmtfile, dotfile ]
	  chmod "go+r" fmtfile
	  return ( fmtfile , fmt , ex )

--------------------------------------------------------------------------

meps, meng :: ToDot a => FilePath -> a -> IO FilePath
meps = mot "-Tps" ".eps"
meng = mot "-Tpng" ".png"

-- | write command line to log file, then execute
system' :: String -> IO ExitCode
system' line = do
    Autolib.Debug.debug $ "system: " ++ line
    system line

mot :: ToDot a => String -> String -> FilePath -> a -> IO FilePath
mot opt ext fname a = do
    let dotfile = fname ++ ".dot"
    let extfile = fname ++ ext
    writeFile dotfile $ show $ toDot $ a
    system' $ unwords 
	   [ progname $ toDotProgram a 
	   , opt
	   , toDotOptions a
	   , dotfile , "-o", extfile ]
    system' $ unwords [ "rm" , dotfile ]
    return extfile

-- | render graphical representation
-- (make dot file, render with neato\/dot, open ghostview)    
display :: ToDot a => a -> IO ()
display a = do
    n <- randomRIO (0,10000 :: Int)
    let fname = "/tmp/display." ++ show n
    epsfile <- meps fname a
    system' $ unwords [ "gv" , epsfile ]
    system' $ unwords [ "rm" , epsfile ]
    return ()

---------------------------------------------------------------------------

