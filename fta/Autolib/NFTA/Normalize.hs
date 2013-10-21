{-# language FlexibleContexts #-}

module Autolib.NFTA.Normalize where

import Autolib.NFTA.Type
import Autolib.NFTA.Ops
import Autolib.FiniteMap

import qualified Data.Map as M

import Autolib.ToDoc 

import qualified Autolib.Relation as Relation

normalize :: ( NFTAC c s, NFTAC c Int )
	  => NFTA c s -> NFTA c Int
normalize a = 
    let fm = M.fromList $ zip (lstates a) [0 .. ]
	fun x = M.findWithDefault 
	      ( error $ unlines
                  [ show (toDoc x)
                  , "Autolib.NFTA.normalize" ++ show a ] )
              x
              fm
    in  statemap fun a

a :: NFTA Int String
a = NFTA
    { alphabet = mkSet [ 0 , 1 , 2 , 3 ]
    , states = mkSet [ "Foo" , "Bar" , "boolean" , "int" ]
    , finals = mkSet [ "int" ]
    , trans = Relation.make [ ( "Foo" , ( 0 , [ ] ) )
                            , ( "Bar" , ( 0 , [ ] ) ) , ( "Bar" , ( 2 , [ "Foo" , "boolean" ] ) )
                            , ( "Bar" , ( 3 , [ "int" , "int" , "int" ] ) )
                            , ( "boolean" , ( 1 , [ "Bar" ] ) )
                            , ( "int" , ( 3 , [ "Foo" , "Bar" , "Foo" ] ) )
                            , ( "int" , ( 3 , [ "Bar" , "Bar" , "boolean" ] ) )
                            ]
    , eps = Relation.make [ ]
    }