-- Main module
-- args: file names
-- action: read TES from file
--         and check whether signature is unary

import TES

import System
import Exception
import IO
import Monad ( when )
import Set

main :: IO ()
main = do 
    argv <- getArgs
    sequence_ $ map handle argv

handle :: FilePath -> IO ()
handle fname = do
    -- hPutStrLn stderr $ "reading file: " ++ fname
    cs <- readFile fname
    let r = read cs :: TES
    -- hPutStrLn stderr $ show r
    when ( at_most_unary r && has_nullary r ) $ do
        putStrLn fname
        print r
  `Exception.catch` \ any -> do
    hPutStrLn stderr $ show any

at_most_unary :: TES -> Bool
at_most_unary tes = and $ do
    s <- setToList $ signature tes
    return $ arity s <= 1

has_nullary :: TES -> Bool
has_nullary tes = or $ do
    s <- setToList $ signature tes
    return $ arity s == 0

    
        
