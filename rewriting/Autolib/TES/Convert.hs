module TES.Convert where

--   $Id$

import TES

import TES.Path
import SRS.Type

import Sets
import Letters

import Util.Sort
import Control.Monad ( guard )
import Data.List ( isPrefixOf, partition, tails )
import Maybe ( isNothing, fromMaybe )
import Data.FiniteMap

to_tes :: SRS Char -> TES
to_tes srs = 
    let x = mknullary "x"
	to_term = foldr ( \ c t -> Node (mkunary [c]) [ t ] ) ( Node x [] )
    in TES 
	{ comment   = "SRS: " ++ ( unwords $ words $ show srs )
	, variables = mkSet [ x ]
	, signature = mkusig $ map (:[]) $ setToList $ letters srs
	, rules     = do
	      (l,r) <- srs
	      return ( to_term l, to_term r )
	}

to_srs :: TES -> SRS Char
-- only works if all arities <= 1
-- WARNING: reduces all symbols to their first letter
-- so these better be distinct (NOT CHECKED)
to_srs t = do
    let to_letter = head . name 
    let to_string ( Node f args ) = case args of
	    [] | f `elementOf` variables t -> [ ] -- name NOT CHECKED
	    [] | True                      -> [ to_letter f ]
            [t] -> to_letter f : to_string t
	    ts  -> error "cannot handle arities > 1"
    (l, r) <- rules t
    return (to_string l, to_string r)

risky_to_srs  :: TES -> SRS Char
-- map signature to distinct characters
risky_to_srs t = do
    let fm = listToFM $ zip ( setToList $ signature t ) [ 'a' .. ]
        to_letter = fromMaybe (error "to_letter") . lookupFM fm
    let to_string fis = do
	    (f, i) <- fis
	    [ to_letter f , head ( show i ) ]
    (l, r) <- rules t
    pr <- paths t r
    -- for each path in rhs (to vars und consts)
    -- find one (eigentlich arbitrary) path in rhs
    let pls = do
	  pl <- paths t l
          guard $ variable pl == variable pr
	       || isNothing (variable pr) -- konstante rechts, links egal
	  return pl
    let ( embed, nonbed ) = 
	    partition ( \ pl -> isSubstringOf (descent pl) (descent pr) ) pls
    pl <- take 1 $ sortBy (negate . length . descent) nonbed 
	     ++ embed -- nur benutzt, falls erstes leer

    return (to_string $ descent pl, to_string $ descent pr)

isSubstringOf x y = or $
    do y' <- tails y
       return $ isPrefixOf x y'
