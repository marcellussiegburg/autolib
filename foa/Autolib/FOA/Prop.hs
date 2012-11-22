{-# language UndecidableInstances #-}
{-# language TemplateHaskell #-}

module Autolib.FOA.Prop where

import Autolib.FOA.Data

import qualified Autolib.Relation as R

import Autolib.ToDoc
import Autolib.Reader
import Data.Typeable
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( guard )

empty :: FOAC c s => FOA c s -> Bool
empty a = null $ runs a

-- | the omega word  prefix . loop^omega
data Run c = Run { prefix :: [c] , loop :: [c] }
    
instance ToDoc (Run c) => Show (Run c) where
    show = render . toDoc

runs :: FOAC c s => FOA c s -> [ Run c ]
runs a = do
    let empties = do 
            p <- S.toList $ states a
            return $ Link { footprint = S.singleton p
                          , example = []
                          , from = p, to = p   
                          }
    let nonempties = do
            s <- M.elems $ paths a 
            S.toList s
    pre <- empties ++ nonempties
    guard $ S.member (from pre) $ starts a
    loo <- nonempties
    guard $ to pre == from loo
    guard $ from loo == to loo
    case acceptance a of
         Muller m -> 
             guard $ S.member (footprint loo) m
    return $ Run { prefix = example pre
                 , loop = example loo   
                 }

data Link c s = Link { footprint :: S.Set s
                   , example :: [ c ]
                   , from :: s, to :: s
                   }


instance ToDoc (Link c s) => Show (Link c s) where
    show = render . toDoc

instance (Eq c, Eq s) => Eq (Link c s) where
    (==) = (==) `on` footprint

instance (Ord c, Ord s) => Ord (Link c s) where
    compare = compare `on` footprint

-- | for each state pair (p,q),
-- the set of sets of letters 
-- that can occur as labels on non-empty 
-- paths from p to q
paths :: FOAC c s 
      => FOA c s 
      -> M.Map (s,s) (S.Set (Link c s))
paths a =
    let base = M.fromListWith S.union $ do
            (p,c,q) <- unCollect $ transitions a
            let l = Link { footprint = S.fromList[p,q]
                         , example = [c]
                         , from = p, to = q
                         }
            return ((p,q), S.singleton l)
        plus m1 m2 = M.unionWith S.union m1 m2
        times m1 m2 = M.fromListWith S.union $ do
            ((p,q), pqs) <- M.toList m1
            ((q',r), qrs) <- M.toList m2
            guard $ q == q'
            let s = S.fromList $ do
                    pq <- S.toList pqs
                    qr <- S.toList qrs
                    return $ Link
                        { footprint = S.union 
                          (footprint pq)(footprint qr)
                        , example = example pq
                           ++ example qr
                        , from = p, to = r   
                        }     
            return ((p,r), s)
        fixpoint base f = 
            let next = f base
            in  if next == base then next 
                else fixpoint next f
    in  fixpoint base $ \ x -> plus x $ times x x

$(derives [makeToDoc] [''Run])
$(derives [makeToDoc] [''Link])
