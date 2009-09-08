{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}

module TestToTransport where

import Data.Autolib.Transport
import Data.Derive.ToTransport

import Data.Derive.JSON
import Text.JSON

data MMaybe a = JJust a | NNothing
    deriving (Eq, Show)

data Frotz = A | B Frotz | C { sub :: Frotz } | D { sub :: Frotz, foo :: Frotz }
    deriving (Eq, Show)

data Hard = Aaa | AAa | AAA { aaa :: Hard }
    deriving (Eq, Show)

data X = () :+: ()

$(derives [makeToTransport] [''MMaybe, ''Frotz, ''Hard, ''X])

t1 :: MMaybe Frotz
t1 = JJust (D { sub = B A, foo = C { sub = A } })

t2 :: [Hard]
t2 = [Aaa, AAa, AAA AAa]
