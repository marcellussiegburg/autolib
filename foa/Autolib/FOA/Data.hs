{-# language DeriveDataTypeable #-}
{-# language ConstraintKinds #-}
{-# language UndecidableInstances #-}

module Autolib.FOA.Data where

import Autolib.Set
import qualified Data.Set as S
import Autolib.FiniteMap hiding ( collect, unCollect )
import qualified Data.Map as M
import qualified Autolib.Relation as R
import Autolib.ToDoc
import Autolib.Reader
import Data.Typeable

type FOAC c s = ( Ord c, Ord s
                , Reader c, Reader s
                , ToDoc c, ToDoc s  
                ) 

data FOA c s = FOA 
    { foa_info :: Doc
    , alphabet :: Set c
    , states :: Set s
    , starts :: Set s
    , acceptance :: Acceptance s
    , transitions :: Transitions c s
    } deriving Typeable

data Transitions c s = 
     Transitions ( FiniteMap c ( R.Type s s ) )

collect ts = Transitions $ M.fromListWith R.plus $ do
    (p,c,q) <- ts
    -- FIXME: relation needs source and target
    return ( c, R.make [(p,q)] )

unCollect (Transitions m) = do
    (c, r) <- M.toList m
    (p,q) <- R.pairs r
    return (p,c,q)

instance (FOAC c s)
    => Reader (Transitions c s) where
    reader = do
        my_reserved "collect" 
        ts <- reader
        return $ collect ts

instance (FOAC c s)
    => ToDoc (Transitions c s) where
     toDoc t = 
         text "collect" <+> toDoc ( unCollect t )

data Acceptance s 
    = Muller (Set (Set s))
    | Buchi (Set s)



