{-# language ConstraintKinds #-}
{-# language UndecidableInstances #-}

module Autolib.FOA.Prop where

import Autolib.FOA.Data

import qualified Autolib.Relation as R

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( guard )

-- | for each state pair (p,q),
-- the set of sets of letters 
-- that can occur as labels on paths from p to q
paths :: FOAC c s 
      => FOA c s 
      -> M.Map (s,s) (S.Set (S.Set c))
paths a =
    let base = M.fromListWith S.union $ do
            (p,c,q) <- unCollect $ transitions a
            return ((p,q), S.singleton $ S.singleton c)
        plus m1 m2 = M.unionWith S.union m1 m2
        times m1 m2 = M.fromListWith S.union $ do
            ((p,q), pq) <- M.toList m1
            ((q',r), qr) <- M.toList m2
            guard $ q == q'
            return ((p,r), S.union pq qr)
        fixpoint base f = 
            let next = f base
            in  if next == base then next 
                else fixpoint next f
    in  fixpoint base $ \ x -> plus x $ times x x
