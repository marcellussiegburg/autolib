{-# language UndecidableInstances #-}
{-# language TemplateHaskell #-}

module Autolib.FOA.Op where

import qualified Autolib.NFA as N
import Autolib.FOA.Data

import qualified Autolib.Relation as R

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( guard )

times :: (N.NFAC c s, FOAC c t
         , Hash t, Show t
         )
      => N.NFA c s -> FOA c t
      -> FOA c (Either s t)
times a b = 
    let l = N.statemap Left a
        r = statemap Right b
        l_accepts_epsilon = not $ S.null
           $ S.intersection (N.starts l) (N.finals l)
    in  FOA { foa_info = text "times" 
               $$ vcat [ N.nfa_info a, foa_info b ]
            , alphabet =  S.union (N.alphabet a)
                                 (alphabet b)
            , states = S.union (N.states l)(states r)
            , starts = S.union (N.starts l) 
                $ if l_accepts_epsilon
                  then starts r else S.empty
            , transitions = collect 
                $ N.unCollect (N.trans l)
                ++ unCollect (transitions r)
                ++ do (p,c,q) <- 
                          N.unCollect (N.trans l)
                      r <- S.toList $ states r
                      return (p,c,r)
                , acceptance = acceptance r      
            }                     

union :: (FOAC c s, FOAC c t) 
      => FOA c s -> FOA c t
      -> FOA c (Either s t)
union a b = 
    let l = statemap Left a
        r = statemap Right b
    in  FOA { foa_info = text "union" 
               $$ vcat [ foa_info a, foa_info b ]
            , alphabet = S.union (alphabet a)
                                 (alphabet b)
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

statemap f a = FOA { 
              foa_info = text "statemap ?" 
                 $$ foa_info a
            , alphabet = alphabet a
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
    in ( statemap ( m M.! ) a ) 
       { foa_info = text "normalize"
           $$ foa_info a }
