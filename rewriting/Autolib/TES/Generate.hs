-- Main

-- $Id$

import SRS.Data
import TES.Convert
import System

target :: FilePath
target = "srsexamples"

main = do
    system $ unwords [ "mkdir", target ]
    mapM_ handle $ zip [1 .. ] public

handle ( k, srs ) = do
    let fname = target ++ "/" ++ show k ++ ".tes"
    let tes = to_tes srs
    print tes
    putStrLn $ "writing file: " ++ fname
    writeFile fname $ show tes ++ "\n"


