module Autolib.NFTA.Cross where

--  $Id$

import Autolib.NFTA.Type
import qualified Autolib.Relation as Relation

import Autolib.Sets
import Autolib.Informed
import Data.FiniteMap

cross :: (NFTAC c s, NFTAC c t) 
      => NFTA c s -> NFTA c t 
      -> NFTA c (s, t)
cross a b = 
    let collect x = addListToFM_C (++) emptyFM $ do
            (p, (c, qs)) <- Relation.pairs $ trans x
            return (c, [(p, qs)])
        combine pqs pqs' = do
            (p, qs) <- pqs
            (p', qs') <- pqs'
            return ((p,p'), zip qs qs')
        fm = mapFM ( \ c pqs -> do (p, qs) <- pqs ; return (p, (c, qs)) )
           $ intersectFM_C combine (collect a) (collect b)
        ss = Autolib.Sets.cross (states a) (states b)
    in  NFTA { nfta_info = funni "cross" [ info a, info b ]
             , alphabet = intersect (alphabet a) (alphabet b)
	     , states = ss
	     , finals = error "Autolib.NFTA.Cross.cross.finals undefined"
	     , trans  = Relation.make $ concat $ eltsFM fm
	     , eps    = Relation.trans
	              $ Relation.make_on (ss, ss) $
                   do (p, p') <- Relation.pairs $ eps a
                      q <- lstates b
                      return ((p,q), (p',q))
               ++  do (q, q') <- Relation.pairs $ eps b
                      p <- lstates a
                      return ((p,q), (p,q'))
	     }

