module TES.Convert where

-- $Id$

import TES
import SRS.Type

import Sets
import Letters

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
