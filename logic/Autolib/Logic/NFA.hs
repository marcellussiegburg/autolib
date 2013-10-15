{-# language FlexibleInstances #-}

module Autolib.Logic.NFA where

import Autolib.Logic.Formula

import Autolib.Logic.Transform ( mso2mso0 )

import Autolib.NFA hiding (letters)
import Autolib.Util.Size
import Autolib.Symbol
import Autolib.Hash
import qualified Data.Set as S

-- | input: MSO_0 formula,
-- output: equivalent automaton

type Alpha = [ SOName ]
type Letter = ( SOName, [ Bool ] )
type Auto = NFA Letter Int

instance Size Bool where size _  = 1
instance Size SOName where size _ = 1
instance Symbol [Bool]
instance Symbol SOName
instance Hash SOName where hash (SOName s) = hash s

t1 = mso2mso0 f1

letters :: Alpha -> Int -> [ Letter ]
letters sigma vars = do
    c <- sigma
    vs <- sequence $ replicate vars [ False, True ]
    return (c, vs)

to_nfa :: Alpha -> Int -> Formula Void Int -> Auto
to_nfa alpha vars f = case f of
    Singleton n -> 
        let sigma = letters  alpha vars
        in  NFA { alphabet = S.fromList sigma
                    , states = S.fromList [ 0, 1 ]
                    , starts = S.fromList [ 0 ]
                    , finals = S.fromList [ 1 ]
                    , trans = collect $ do
                         l @ (c, vs) <- sigma
                         if vs !! n then [ (0,l,1) ] else [ (0,l,0), (1,l,1) ]
                    }
    
    ExistsSO fun -> undefined

    _ -> error $ "to_nfa: " ++ show f
