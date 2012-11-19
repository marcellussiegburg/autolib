{-# language UndecidableInstances #-}
{-# language TemplateHaskell #-}

module Autolib.FOA.Op where

import Autolib.FOA.Data

import qualified Autolib.Relation as R

import Autolib.ToDoc
import Autolib.Reader

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( guard )

union :: (FOAC c s, FOAC c t) 
      => FOA c s -> FOA c t
      -> FOA c (Either s t)
union a b = 
    let l = statemap Left a
        r = statemap Right b
    in  FOA { alphabet = alphabet a
            , states = S.union (states l)(states r)
            , starts = S.union (starts l)(starts r)
            , transitions = collect $
                    unCollect (transitions l)
                 ++ unCollect (transitions r)
            , acceptance = 
                case (acceptance l ,acceptance r) of
                   ( Muller ml, Muller mr ) ->
                      Muller $ S.union ml mr
            }          

statemap f a = FOA { alphabet = alphabet a
            , states = S.map f $ states a
            , starts = S.map f $ starts a
            , transitions = collect $ do
                (p,c,q) <- unCollect $ transitions a
                return (f p, c, f q)
            , acceptance = case acceptance a of
                Muller m -> Muller $ S.map (S.map f) m
            }    

normalize :: FOAC c s => FOA c s -> FOA c Int
normalize a = 
    let m = M.fromList 
          $ zip ( S.toList $ states a ) [ 0 .. ]
    in statemap ( m M.! ) a
