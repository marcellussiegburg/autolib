-- $Header$

module Main where

import Exp
import ExpRead
import Statement
import Inter
import Env
import Shortest
import NFA
import Parsec

--------------------------------------------------------------------------

loop :: E -> [ Statement ] -> IO ()
loop env [] = return ()
loop env (t : ts) = do
     print t
     env' <- eval env t
     loop env' ts

eval :: E -> Statement -> IO E
eval env (Print x) = do
     putStrLn $ "-- print statement"
     let v = inter env x
     putStr " == "
     inform v
     return env
eval env (Let n x) = do
     putStrLn $ "-- assignment statement"
     let v = inter env x
     let env' = add env (n, v)
     putStr $ n ++ " == "
     inform v 
     return env'

--------------------------------------------------------------------------

inform :: NFA Int -> IO ()
inform a = do
    putStrLn $ "-- minimal det automaton:"
    print a
    putStrLn $ "-- first few words of its language:"
    print $ take 10 $ accepted a

--------------------------------------------------------------------------

main :: IO ()
main = do
     putStrLn $ "-- $Id$"
     putStrLn $ "-- welcome"
     input <- getContents
     case parse program "<stdin>" input of
	  Right stats -> do
		putStrLn $ "-- input OK"
		loop std stats
		putStrLn $ "-- done"
	  Left msg -> do
	        putStrLn $ "-- parse error"
		print msg
