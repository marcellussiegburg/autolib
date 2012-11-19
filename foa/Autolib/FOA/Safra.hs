{-# language UndecidableInstances #-}
{-# language TemplateHaskell #-}
{-# language DatatypeContexts #-}

-- | cf. description in Section 2.9 of
-- http://www.informatik.uni-bremen.de/tdki/lehre/ws09/automata/
-- also in W. Thomas: Languages, Automata, and Logic,
-- Section 5.2, in: Handbook of Formal Languages,
-- Volume 3, pp. 419

module Autolib.FOA.Safra where

import Autolib.FOA.Data

import qualified Autolib.Relation as R

import Autolib.ToDoc
import Autolib.Reader

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( guard )


-- | invariants: for each node with label S 
-- and children with labels S_1 .. S_k:
-- 1. union of  S_1 .. S_k is strict subset of S
-- 2. i /= j => S_i and S_j are disjoint
-- 0.  S is nonempty

data Ord s => Tree s = 
    Node { mark :: Bool, label :: S.Set s 
         , children :: [ Tree s ]
         }
    deriving ( Eq, Ord )

step :: Ord s 
     => S.Set s -> R.Type s s -> Tree s -> Tree s    
step f rel = step6 . step5 . step4
           . step3 rel . step2 f . step1

-- | remove mark
step1 :: Ord s => Tree s -> Tree s
step1 t = 
    t { mark = False
      , children = map step1 $ children t
      }

step2 :: Ord s => S.Set s -> Tree s -> Tree s
step2 f t = 
    let i = S.intersection (label t) f
    in  t { children = map (step2 f) (children t)
               ++ [ Node { mark = False
                      , label = i, children = [] } ]
          }            

-- | powerset construction.
-- | first arg is the transition relation
-- for the letter sigma 
step3 :: Ord s => R.Type s s -> Tree s -> Tree s
step3 rel t = 
    t { label = R.simages rel $ label t 
      , children = map (step3 rel) $ children t
      }

-- | horizontal merge
step4 :: Ord s => Tree s -> Tree s
step4 t = 
    let drop seen [] = []
        drop seen (c:cs) = 
            c { label = S.difference (label c) seen }
            : drop (S.union (label c) seen) cs
    in  t { children = drop S.empty
              $ map step4 $ children t }

step5 :: Ord s => Tree s -> Tree s
step5 t = 
    t { children = filter (not . S.null . label)
                 $ map step5 $ children t }

-- | vertical merge
step6 :: Ord s => Tree s -> Tree s
step6 t =
    let u = S.unions $ map label $ children t
    in  if S.isSubsetOf (label t) u
        then  t { mark = True, children = [] }
        else  t { children = map step6 $ children t }
    
initial i = 
    Node { mark = False, label = i , children = [] }

all_labels t = S.insert (label t) 
    $ S.unions  ( map all_labels $ children t )

occurs :: Ord s => S.Set s -> Tree s -> Bool
occurs s t = 
    (s == label t)
    && or ( map (occurs s) $ children t )

occurs_marked :: Ord s => S.Set s -> Tree s -> Bool
occurs_marked s t = 
    (mark t && s == label t)
    && or ( map (occurs_marked s) $ children t )

finals :: Ord s => [ Tree s ] -> S.Set (S.Set (Tree s))
finals states = S.fromList $ do
    k <- S.toList $ S.unions $ map all_labels states
    let ts = filter ( occurs k ) states
    guard $ or $ map ( occurs_marked k ) ts
    return $ S.fromList ts

safra :: FOAC c s
      => FOA c s -- ^ Buchi
      -> FOA c (Tree s) -- ^ Muller deterministic
safra a | Buchi f <- acceptance a =
    let 
        hull done [] = []
        hull done (t:odo) = 
            if S.member t done then hull done odo
            else let here = do
                        c <- S.toList $ alphabet a
                        let t' = step f ( transitions_for c a ) t
                        return ( t, c, t' )
                     later = hull (S.insert t done)
                           $ map (\(_,_,t')->t') here 
                           ++ odo   
                 in  here ++ later
        ini = initial $ starts a 
        trs = hull S.empty [ ini ]
        sts = S.fromList $ do (p,c,q) <- trs ; [ p,q]
    in  FOA { alphabet = alphabet a
            , states = sts, starts = S.singleton ini
            , transitions = collect trs 
            , acceptance = 
                Muller $ finals $ S.toList sts
            }


$(derives [makeToDoc, makeReader] [''Tree])

instance ToDoc (Tree s) => Show (Tree s) where
    show = render . toDoc

