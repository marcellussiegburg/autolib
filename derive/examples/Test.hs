-- {-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell, TypeSynonymInstances #-}

module Test where

import Data.Derive.Reader
import Data.Derive.ToDoc

import Autolib.Reader hiding (makeReader)
import Autolib.ToDoc hiding (makeToDoc)
import Autolib.Multilingual.Doc

import Data.DeriveTH (derives)

data MMaybe a = JJust a | NNothing
    deriving Eq

data Frotz = A | B Frotz | C { sub :: Frotz } | D { sub :: Frotz, foo :: Frotz }
    deriving Eq

data Hard = Aaa | AAa | AAA { aaa :: Hard }
    deriving Eq

instance Eq Doc where _ == _ = True

data HasInfo = HA { a_info :: Doc }
             | HB { b_info :: Doc, b_data :: Int }
             | HC { c_data :: Int, c_info :: Doc }
             | HD { a_da :: Int, a_info :: Doc, a_ta :: Int }
    deriving Eq

$(derives [makeToDoc, makeReader] [''MMaybe, ''Frotz, ''Hard, ''HasInfo])

t1 :: MMaybe Frotz
t1 = JJust (D { sub = B A, foo = C { sub = A } })

t2 :: [Hard]
t2 = [Aaa, AAa, AAA AAa]

t3 :: [HasInfo]
t3 = [HA empty, HB empty 2, HC 1 empty, HD 1 empty 3]

q :: (Reader a, ToDoc a, Eq a) => a -> Maybe Doc
q a = case runParser reader () "" (showDoc $ toDoc a) of
    Right a' -> if a == a' then Nothing else Just (err a')
    Left e   -> Just (text (show e))
  where
    err a' = vcat [text "wrong answer:",
                   nest 4 $ toDoc a',
                   text "expected:",
                   nest 4 $  toDoc a]
