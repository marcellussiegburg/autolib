module Autolib.Exp.Env where

--  $Id$

import Autolib.FiniteMap
import Data.Set

type Env a = FiniteMap String a

make :: [ (String, a) ] -> Env a
make = listToFM

look :: Env a -> String -> Maybe a
look = lookupFM

plus :: Env a -> Env a -> Env a
plus = plusFM_C (error "Env.plus")

keys :: Env a -> Set String 
keys = mkSet . keysFM

add :: Env a -> (String, a) -> Env a
add e (n, v) = plus e (make [(n,v)])
