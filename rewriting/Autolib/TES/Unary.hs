-- Main module
-- args: file names
-- action: read TES from file
--         and check whether signature is unary

import TES

import System
import Exception
import IO
import Control.Monad ( when )
import Data.Set

main :: IO ()
main = do 
    argv <- getArgs
    sequence_ $ map handle argv

handle :: FilePath -> IO ()
handle fname = do
    -- hPutStrLn stderr $ "reading file: " ++ fname
    hPutStr stderr $ ". " ; hFlush stderr
    cs <- readFile fname
    let r = read cs :: TES
    -- hPutStrLn stderr $ show r
    when ( at_most_unary r ) $ do
        putStrLn $ "\nunary: " ++ fname
        -- print r
	writeFile ( fname ++ ".unary" ) $ show r ++ "\n"
  `Exception.catch` \ any -> do
      -- hPutStrLn stderr $ fname ++ show any
      hPutStrLn stderr $ "\n?: " ++ fname ++ "\n"


    
        
