-- | smallest trees that belong to language
--  $Id$

module Autolib.NFTA.Shortest 

( shortest
, shortest0
)

where

import Autolib.TES.Type
import Autolib.NFTA.Type
import Autolib.NFTA.Basic
import Autolib.NFTA.Trim
import Autolib.NFTA.Normalize
import Autolib.NFTA.Epsilon
import qualified Autolib.Relation as Relation

import Autolib.TES.Identifier

import Data.Array
import Control.Monad ( guard )

shortest :: NFTAC c s
	 => NFTA c s
	 -> [ Term () c ]
shortest aut0 = do
    let aut = trim $ normalize $ uneps aut0
        out = shortest0 aut
    s <- [ 0 .. ]
    p <- lfinals aut
    ( out ! p ) !! s

-- | array maps state to list of list of accepted terms
-- for each such list-of-lists tss, 
-- tss !! i  contains all terms of size i
-- (so xss !! 0 is empty, etc.)
-- should only be applied to trim and epsilon free automata,
shortest0 :: NFTAC c Int 
	  => NFTA c Int 
          -> Array Int [ [Term () c] ]
shortest0 aut = out where
    ps = lstates aut
    out = array (head ps, last ps) $ do
            p <- ps
            return ( p, do
                s <- [ 0 .. ]
                return $ do
		    ( f, qs ) <- setToList $ Relation.images ( trans aut ) p
		    ts <- combine (s - 1) $ do q <- qs ; return $ out ! q
                    return $ Node f ts
              )

combine :: Int -> [[[a]]] -> [ [a] ]
combine s [] = do
    guard $ 0 == s
    return []
combine s [ ts ] = do
    guard $ 0 <= s
    t <- ts !! s
    return [ t ]
combine s (ts : tss) = do
    guard $ s > length tss
    (st, us) <- zip [ 0 .. s - length tss ] ts
    u <- us
    vs <- combine (s - st) tss
    return $ u : vs

