{-# language DeriveDataTypeable #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language DatatypeContexts #-}
{-# language UndecidableInstances #-}
{-# language TemplateHaskell #-}

module Autolib.FOA.Data where

import Autolib.Set
import qualified Data.Set as S
import Autolib.FiniteMap hiding ( collect, unCollect )
import qualified Data.Map as M
import qualified Autolib.Relation as R
import Autolib.ToDoc
import Autolib.Reader
import Data.Typeable

class ( Ord c, Ord s
                , Reader c, Reader s
                , ToDoc c, ToDoc s  
                ) => FOAC c s

instance ( Ord c, Ord s
                , Reader c, Reader s
                , ToDoc c, ToDoc s  
                ) => FOAC c s


data FOAC c s => FOA c s = FOA 
    { foa_info :: Doc
    , alphabet :: Set c
    , states :: Set s
    , starts :: Set s
    , transitions :: Transitions c s
    , acceptance :: Acceptance s
    } deriving Typeable

data Transitions c s = 
     Transitions ( FiniteMap c ( R.Type s s ) )

data (Ord s) => Acceptance s 
    = Muller (Set (Set s))
    | Buchi (Set s)

-- | http://www.informatik.uni-bremen.de/tdki/lehre/ws09/automata/

ex55 :: FOA Char Int
ex55 = FOA
    { alphabet = S.fromList "ab"
    , states = S.fromList [0,1]
    , starts = S.fromList [0]
    , transitions = collect 
        [ (0, 'a', 0), (0, 'b', 1)
        , (1, 'b', 1), (1, 'a', 0)
        ]
    , acceptance = Muller $ S.singleton $ S.singleton 0
    }

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

$(derives [makeToDoc, makeReader] [''FOA, ''Acceptance])

instance ToDoc (FOA c s) => Show (FOA c s) where
    show = render . toDoc




