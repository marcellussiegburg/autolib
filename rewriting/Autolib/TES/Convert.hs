module Autolib.TES.Convert where

--   $Id$

import TES

import Autolib.TES.Path
import Autolib.TES.Identifier
import Autolib.TES.Sexp
import SRS.Type

import Sets
import Letters

import Util.Sort
import Control.Monad ( guard )
import Data.List ( isPrefixOf, partition, tails )
import Maybe ( isNothing, fromMaybe )
import Data.FiniteMap

to_trs :: Symbol c 
       => SRS c -> TRS Identifier c
to_trs srs = RS 
	{ annotations  = [ wrap "SRS:" $ srs ]
	, theory = Nothing
	, strategy = Nothing
	-- , variables = mkSet [ x ]
	-- , signature = mkusig $ map (:[]) $ setToList $ letters srs
	, rules     = do
	      (l,r) <- srs
	      return ( to_term l, to_term r )
	, separate = False
	}

to_term = foldr ( \ c t -> Node c [ t ] ) ( Var $ mknullary "x" )

to_srs :: Symbol c
       => TRS v c 
       -> SRS c
-- only works if all arities <= 1
-- WARNING: reduces all symbols to their first letter
-- so these better be distinct (NOT CHECKED)
to_srs t = do
    let to_string ( Var v ) = [ ] -- name NOT CHECKED
	to_string ( Node f args ) = case args of
	    [] | True                      -> [ f ]
            [t] -> f : to_string t
	    ts  -> error "cannot handle arities > 1"
    (l, r) <- rules t
    return (to_string l, to_string r)

risky_to_srs  :: TES -> SRS Char
-- map signature to distinct characters
risky_to_srs t = do
    let fm = listToFM $ zip ( setToList $ symbols $ rules t ) [ 'a' .. ]
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
