module Autolib.Util.Edit where

-- -- $Id$

import Autolib.Util.Zufall
import Control.Monad

import Autolib.Util.Zufall ( eins )

cut :: Int -> Int -> [a] -> [a]
cut from to w = 
    let (pre, post) = splitAt from w
    in	pre ++ drop (to - from) post

mirror :: Int -> Int -> [a] -> [a]
mirror from to w = 
    let (pre, post) = splitAt from w
	(here, there) = splitAt (to - from) post
    in	pre ++ reverse here ++ there

insert_slice :: Int -> Int -> Int -> [a] -> [a]
insert_slice from to at w =
    let slice = take (to - from) $ drop from w
	(here, there) = splitAt at w
    in	here ++ slice ++ there

max_edit = 3 :: Int
    
some_cut :: RandomC m => [a] -> m [a]
some_cut w = if length w < max_edit then return w else do
    d <- randomRIO ( 1 , max_edit )
    i <- randomRIO ( 0 , length w - d )
    return  $ cut i (i+d) w
    
some_mirror :: RandomC m => [a] -> m [a]
some_mirror w = if length w < max_edit then return w else do
    d <- randomRIO ( 1 , max_edit )
    i <- randomRIO ( 0 , length w - d )
    return  $ mirror i (i+d) w

some_insert_slice :: RandomC m => [a] -> m [a]
some_insert_slice w = if length w < max_edit then return w else do
    d <- randomRIO ( 1 , max_edit )
    i <- randomRIO ( 0 , length w - d )
    k <- randomRIO (0    , length w - 1)
    return $ insert_slice i (i+d) k w

edit :: RandomC m => [a] -> m [a]
edit w = do
    f <- eins 
       [ some_cut 
       , some_insert_slice 
       , some_mirror 
       ]
    f w


edits :: RandomC m => [a] -> m [a]
edits w = do
    k <- randomRIO (1, max_edit) -- ??
    foldM ( \ u _ -> edit u ) w $ replicate k ()
    
    

