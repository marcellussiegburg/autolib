module Autolib.Dot.Dotty where

--  $Id$


import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Hash
import qualified Autolib.Output as Output

import Autolib.Dot.Dot
import qualified Autolib.Dot.Graph
import Autolib.Size

import IO
import System

---------------------------------------------------------------------

trusted_layouters :: [ FilePath ]
trusted_layouters = do
    prog <- [ "dot", "twopi", "neato" ]
    prefix <- [ "", "/usr/bin/", "/usr/local/bin/" ]
    return $ prefix ++ prog

dotty :: ( Hash a, ToDot a )
      => String -- ^ layouter
      -> a
      -> Reporter ()
dotty layouter a = do

    let it = toDot a
	pre =  show $ abs $ hash a 

    let dotfile = pre ++ ".dot" 
        pngfile = pre ++ ".png"
        giffile = pre ++ ".gif"
	epsfile = pre ++ ".eps"
    output $ Output.Link dotfile

    silent $ assert ( layouter `elem` trusted_layouters )
                    ( text "layout program is trusted?" )

    execute $ do
          writeFile dotfile $ show it ++ "\n\n"
          system' $ unwords [ layouter , "-s"
               , "-Grankdir=LR", "-Tpng"
               , "-o", pngfile
               , dotfile
               ]
          system' $ unwords [ layouter , "-s"
               , "-Grankdir=LR", "-Tps"
               , "-o", epsfile
               , dotfile
               ]
          return ()
    output $ Output.Image  pngfile
    output $ Output.Link  epsfile

