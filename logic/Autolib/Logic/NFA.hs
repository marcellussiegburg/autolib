{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}

module Autolib.Logic.NFA where

import Autolib.Logic.Formula.Name
import Autolib.Logic.Formula.SO
import Autolib.Logic.Formula.FO (f1)
import Autolib.Logic.Transform (s1)

import Autolib.NFA as NFA hiding (letters) 
import Autolib.NFA.Ops as NFA
import Autolib.Util.Size
import Autolib.Symbol
import Autolib.Hash

import qualified Data.Set as S
import Control.Monad ( guard )

-- | input: MSO_0 formula,
-- output: equivalent automaton

type Alpha = [ Name ]
type Letter = ( Name, [ Bool ] )
type Auto = NFA Letter Int

instance Size Bool where size _  = 1
instance Size Name where size _ = 1
instance Symbol [Bool]
instance Symbol Name


letters :: Alpha -> Int -> [ Letter ]
letters sigma vars = do
    c <- sigma
    vs <- sequence $ replicate vars [ False, True ]
    return (c, vs)

to_nfa :: Alpha -> Int -> Formula  -> Auto
to_nfa alpha vars f = case f of
    Succ l r ->
        let sigma = letters  alpha vars
        in  NFA { alphabet = S.fromList sigma
                , states = S.fromList [ 0, 1, 2 ]
                , starts = S.fromList [ 0 ]
                , finals = S.fromList [ 2 ]
                , trans = collect $ do
                    x @ (c, vs) <- sigma
                    case (vs !! l, vs !! r) of
                        (False,False) -> 
                            [(0,x,0), (2,x,2)]
                        (True,False) -> [ (0,x,1) ]
                        (False,True) -> [ (1,x,2) ]
                        (True,True) -> []
                }

    Less l r ->
        let sigma = letters  alpha vars
        in  NFA { alphabet = S.fromList sigma
                , states = S.fromList [ 0, 1, 2 ]
                , starts = S.fromList [ 0 ]
                , finals = S.fromList [ 2 ]
                , trans = collect $ do
                    x @ (c, vs) <- sigma
                    case (vs !! l, vs !! r) of
                        (False,False) -> 
                            [(0,x,0), (1,x,1), (2,x,2)]
                        (True,False) -> [ (0,x,1) ]
                        (False,True) -> [ (1,x,2) ]
                }

    Letter c n -> 
        let sigma = letters  alpha vars
        in  NFA { alphabet = S.fromList sigma
                , states = S.fromList [ 0 ]
                , starts = S.fromList [ 0 ]
                , finals = S.fromList [ 0 ]
                , trans = collect $ do
                     x @ (d, vs) <- sigma
                     guard $ (vs!!n) <= (c == d) 
                     return (0,x,0)
                }

    Singleton n -> 
        let sigma = letters  alpha vars
        in  NFA { alphabet = S.fromList sigma
                , states = S.fromList [ 0, 1 ]
                , starts = S.fromList [ 0 ]
                , finals = S.fromList [ 1 ]
                , trans = collect $ do
                     x @ (c, vs) <- sigma
                     if vs !! n 
                         then [ (0,x,1) ] 
                         else [ (0,x,0), (1,x,1) ]
                }

    Not f -> NFA.complement (letters alpha vars )
          $ to_nfa alpha vars f
    Or x y -> NFA.normalize_union 
        (to_nfa alpha vars x) (to_nfa alpha vars y)
    And x y -> NFA.normalize_intersection
        (to_nfa alpha vars x) (to_nfa alpha vars y)
    Implies x y -> to_nfa alpha vars $ Or (Not x) y
    
    Exists fun -> 
        let a = to_nfa alpha (1 + vars) ( fun vars )
            sigma = letters  alpha vars
        in  a { alphabet = S.fromList sigma
              , trans = collect $ do
                 (p, (c, vs), q) <- unCollect $ trans a
                 return (p, (c, init vs), q)
              }
    Forall fun -> to_nfa alpha vars $
        Not $ Exists $ \ x -> Not $ fun x

    _ -> error $ "to_nfa: " ++ show f
