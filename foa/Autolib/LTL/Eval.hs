{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}

module Autolib.LTL.Eval where

import Autolib.LTL.Type
import Autolib.FOA.Data
import Autolib.FOA.Op
import Autolib.FOA.Prop

import qualified Autolib.NFA as N
import qualified Autolib.NFA.Basic as N
import Autolib.Symbol 

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Hash

import qualified Data.Map as M
import qualified Data.Set as S

eval :: [Name] -> Formula -> FOA [ Boo ] Int
eval ns f = 
    let vmap = M.fromList $ zip ns [0..]
        num n = vmap M.! n
        alpha = letters ns
        evil f = case f of
            Nullary (Constant c) -> FOA
                { foa_info = toDoc f
                , alphabet = S.fromList alpha
                , states = S.singleton 0
                , starts = S.singleton 0
                , transitions = collect 
                   $ do l <- alpha 
                        guard c
                        return (0,l,0)
                , acceptance = Muller 
                    $ S.singleton $ S.singleton 0
                }
            Variable v -> FOA
                { foa_info = toDoc f    
                , alphabet = S.fromList $ alpha
                , states = S.fromList [0,1]
                , starts = S.singleton 0
                , transitions = collect 
                     $ do l <- alpha
                          guard 
                              $ l !! num v == Boo True
                          return (0, l, 1)
                   ++  do l <- alpha
                          return (1, l, 1)
                , acceptance = Muller 
                     $ S.singleton $ S.singleton 1
                }       
            Unary Next g ->     
                let a = N.sigma alpha
                in  normalize $ times a $ evil g
            Unary Always g -> 
                evil $ Unary Not $ Unary Eventually
                     $ Unary Not g
            Unary Eventually g ->
                let a = N.sigmastar alpha
                in  normalize $ times a $ evil g
            Unary Not g -> 
                normalize $ complement $ evil g
            Binary Or g h ->
                normalize $ union (evil g) (evil h)
            Binary And g h ->
                evil $ Unary Not
                     $ Binary Or ( Unary Not g )
                                 ( Unary Not h )
            Binary Implies g h ->
                evil $ Binary Or ( Unary Not g ) h
    in  evil f

letters ns = 
    sequence $ replicate (length ns) 
             [ Boo False, Boo True ]

-- | Boolean, with representation as 0/1
newtype Boo = Boo Bool deriving (Eq, Ord, Enum)
instance ToDoc Boo where
    toDoc (Boo b) = toDoc $ fromEnum b
instance Reader Boo where
    reader = do my_reserved "0" ; return $ Boo False
         <|> do my_reserved "1" ; return $ Boo True
instance Symbol [Boo ]
instance Size Boo where size = const 1
instance Hash Boo where hash (Boo f) = hash f
instance Show Boo where show = render . toDoc

