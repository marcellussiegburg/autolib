module Main where

-- -- $Id$

import Exp.Type

import Exp.Statement
import Exp.Inter
import Exp.Env

import NFA.Shortest
import NFA.Type
import NFA.Dot
import NFA.Compact
import NFA.Basic

import Parsec ( parse, Parser )
import System.Console.Readline

--------------------------------------------------------------------------

eval :: E -> Statement -> IO E
eval env (Print x) = do
     putStrLn $ "-- print statement"
     let v = inter env x
     putStr " == "
     inform v
     return env
eval env (Display x) = do
     putStrLn $ "-- display statement"
     let v = inter env x
     putStr " == "
     inform v
     display $ sequential_compact v
     return env
eval env (Let n x) = do
     putStrLn $ "-- assignment statement"
     let v = inter env x
     let env' = add env (n, v)
     putStr $ n ++ " == "
     inform v 
     return env'

--------------------------------------------------------------------------

inform :: NFA Char Int -> IO ()
inform a = do
    putStrLn $ "-- minimal det automaton:"
    print a
    putStrLn $ "-- first few words of its language:"
    print $ take 10 $ accepted a

--------------------------------------------------------------------------

main :: IO ()
main = do
     putStrLn $ "-- -- $Id$"
     putStrLn $ "-- welcome"
     loop $ make [ ( "Eps", NFA.Basic.epsilon) ]

loop e = do
    mst <- consume statement Quit
    case mst of
        Nothing -> loop e -- again
	Just Quit -> return ()
        Just st -> do
	    e' <- eval e st
	    loop e'

-----------------------------------------------------------------------

consume :: Parser a -> a -> IO ( Maybe a )
-- read lines until complete parse is obtained
-- or empty line is input (returns Nothing)
consume p def = helper "" where
    helper accu = do
        let prompt = if null accu then "Loop$ " else "Loop> "
        mcs <- readline prompt
        case mcs of
	   Nothing -> return $ Just def
	   Just "" -> return $ Nothing
           Just cs -> do
	      addHistory cs
              let accu' = accu ++ cs
	      case parse p "<stdin>" accu' of
	          Right x -> do
		      putStrLn $ "-- input OK"
		      return $ Just x
		  Left msg -> do
		      putStrLn $ "-- parser says: " ++ show msg
		      putStrLn $ "-- incomplete line? continue ... (ENTER = cancel)"
		      helper accu' 

    
    
	     