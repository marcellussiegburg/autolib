module Autolib.CGI.Store 

( Store
, empty, push, open, close, collect
)

where

import Autolib.CGI.Container

-- | storage is a stack of containers
data Store a = Store { contents :: [ Container a ] }

empty :: Store a
empty = Store { contents = [] }

-- | add one element to the topmost container
push :: a -> Store a -> Store a
push x s =
    let (c : cs) = contents s
    in  s { contents = add x c : cs }

-- | add an empty container with the given combining function on top
open :: ([a] -> a) 
     -> Store a 
     -> Store a
open fun s = 
    s { contents = fresh fun : contents s }

-- | dump the topmost container, combine its elements 
-- and insert into next container, which then becomes topmost.
close :: Store a -> Store a
close s = 
    let ( c : d : ds ) = contents s
    in  s { contents = add (dump c) d : ds }

-- | close all containers
collect :: Store a -> a
collect s = case contents s of
    [ c ] -> dump c
    c : cs -> collect $ close s
