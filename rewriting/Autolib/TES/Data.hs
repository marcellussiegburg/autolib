{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

-- | implements term rewriting systems
-- as represented in the .trs-format

module Autolib.TES.Data where

--  $Id$

import Autolib.Symbol
import Autolib.Size

import Autolib.TES.Term
import Autolib.TES.Position (syms, vars)
import Autolib.TES.Rule
import Autolib.TES.Identifier

import qualified Autolib.SRS.Rule -- only for instances


import Autolib.Set
import Autolib.Hash

import Autolib.ToDoc
import Autolib.Reader
import Data.List ( partition )
import Autolib.FiniteMap
import Control.Monad ( guard )

import Autolib.TES.Sexp
import Autolib.TES.Parsec
import Autolib.TES.Position (symsl)

import Autolib.Letters

import Data.Typeable

data RS c t  = RS
	 { annotations :: [ Sexp ]
	 , theory    :: Maybe [ Sexp ]
	 , strategy  :: Maybe [ Sexp ]

	 , original_variables :: [c] -- ^ order as declared, 
	   -- this is needed to work around a restriction in Rainbow

	 , signature :: Set c -- ^ all symbols (not including variables, I hope)

	 , separate :: Bool
	 , rules :: [ Rule t ]
	 }
    deriving ( Eq, Ord, Typeable )

instance ( Eq c, Eq t, Hash t ) => Hash ( RS c t ) where
    hash rs = hash ( rules rs )

instance Size t => Size (RS c t) where
    size rs = size ( rules rs )

{-# DEPRECATED rules "use strict_rules or non_strict_rules" #-}

strict_rules rs = do
    Rule { lhs = l, strict = True, rhs = r } <- rules rs
    return (l, r)

non_strict_rules rs = do
    Rule { lhs = l, strict = False, rhs = r } <- rules rs
    return (l, r)

all_rules rs = do
    strict_rules rs ++ non_strict_rules rs

with_rules :: Letters t c
	   => RS c t' -> [ Rule t ] -> RS c t
with_rules srs rs = 
     RS
	 { annotations = annotations srs
	 , theory  = theory srs
	 , strategy  = strategy srs
	 , separate = separate srs
	 , original_variables = original_variables srs
     	 , signature = letters rs
	 , rules = rs
	 }

from_strict_rules :: Letters t c
	   => Bool -> [ ( t,t ) ] -> RS c t
from_strict_rules sep rs = 
    let rools = do
          (l, r) <- rs
	  return $ Rule { lhs = l, strict = True, rhs = r }
    in  RS { annotations = []
		   , theory = Nothing
		   , strategy = Nothing
		   , original_variables = []
		   , signature = letters rools
		   , separate = sep
		   , rules = rools
		   }

from_srs :: Letters t c
	   => [ ( t,t ) ] -> RS c t
from_srs = from_strict_rules True



type TRS v c = RS c ( Term v c )

type TES = TRS Identifier Identifier

type SES = RS Identifier [ Identifier ]

instance ( Ord c , Letters t c ) => Letters ( RS c t ) c where
    letters rs = unionManySets $ do 
        r <- rules rs
	return $ letters (lhs r) `union` letters (rhs r)

variables rs =  unionManySets $ do 
        r <- rules rs
	return $ vars (lhs r) `union` vars (rhs r)

lhss :: RS c t -> [ t ]
lhss trs = strict_lhss trs ++ non_strict_lhss trs

rhss :: RS c t -> [ t ]
rhss trs = strict_rhss trs ++ non_strict_rhss trs

strict_lhss :: RS c t -> [ t ]
strict_lhss trs = do (l,r) <- strict_rules trs ; return l

non_strict_lhss :: RS c t -> [ t ]
non_strict_lhss trs = do (l,r) <- non_strict_rules trs ; return l

strict_rhss :: RS c t -> [ t ]
strict_rhss trs = do (l,r) <- strict_rules trs ; return r

non_strict_rhss :: RS c t -> [ t ]
non_strict_rhss trs = do (l,r) <- non_strict_rules trs ; return r


instance ( ToDoc (Rule t), Symbol c ) 
	 => ToDoc ( RS c t ) where
    toDoc t = vcat [ vcat $ map toDoc $ annotations t 
		   , case theory t of 
			  Just x -> toDoc $ List $ Leaf ("THEORY")   :  x 
			  Nothing -> empty
		   , case strategy t of 
		          Just x -> toDoc $ List $ Leaf ("STRATEGY") :  x 
			  Nothing -> empty
		   -- , toDoc ( wrap "VAR" $ setToList $ variables t )
		   -- , toDoc $ ( if separate t then wrap_sep "," else wrap )
                   --          "RULES" $ rules t 
                   , Autolib.ToDoc.parens 
                   $ text "RULES" <+> ( vcat $ rundown (separate t) $ rules t )
                   ]

rundown :: ToDoc ( Rule t ) 
        => Bool -> [ Rule t ] -> [ Doc ]
rundown sep [] = []
rundown sep [ r ] = [ toDoc r ]
rundown sep ( r : rs ) 
    = ( toDoc r <+> if sep then Autolib.ToDoc.comma else Autolib.ToDoc.empty )
    : rundown sep rs

instance Reader SES where
    readerPrec p = do
        ses <- plain_reader
	return $ ses { separate = True }

instance Reader TES where
    readerPrec p = do
	tes <- plain_reader
        return $ check_arities
	       --  $ repair_signature
	       $ repair_variables 
	       $ tes { separate = False }

plain_reader :: Reader (Rule t) => Parser ( RS c t )
plain_reader =  do
        whiteSpace trs
	let trs0 = RS { annotations = []
		     , theory = Nothing
		     , strategy = Nothing
		     , original_variables = []
		     , signature = emptySet
		     , rules = []
		     , separate = False
		     }
        fs <- many line
	return $ foldr (.) id fs trs0

line :: Reader (Rule t) =>  Parser ( RS c t -> RS c t )
line = Autolib.TES.Parsec.parens Autolib.TES.Parsec.trs $  do
     f <- identifier  Autolib.TES.Parsec.trs 
     case f of
	  "RULES" -> do
	      rs <- many $ do
		   r <- reader
		   option "?" my_comma
		   return r
	      return $ \ t -> t { rules = rules t ++ rs }
	  "THEORY" -> do
              args <- many reader
	      return $ \ t -> t { theory   = Just args
				}
	  "STRATEGY" -> do
              args <- many reader
	      return $ \ t -> t { strategy = Just args
				}
	  _ -> do
              args <- many reader
	      return $ \ t -> t { annotations = annotations t 
			            ++ [ List $ Leaf f  : args ]
				}

repair_variables :: TES -> TES
repair_variables trs =
    let vhead (List (Leaf "VAR" : _ )) = True 
        vhead _ = False
	ordered_vs = do 
		List ( Leaf "VAR" : xs ) <- annotations trs
		Leaf x <- xs
	        return $ mknullary x
        vs = mkSet $ ordered_vs
	-- change (some) nullary ids to vars
	xform ( Node c [] ) | c `elementOf` vs = Var c
	xform ( Node c args ) = Node c ( map xform args )
	-- apply to rules
        rs = do rule <- rules trs ; return $ rule { lhs = xform $ lhs rule
						  , rhs = xform $ rhs rule
						  }
	sig = sfilter ( \ s -> not (s `elementOf` vs)) $ symbols rs
    in  trs { rules = rs
	    , original_variables = ordered_vs
	    , signature = sig
           }

-- | add nullary symbol (if not already there) to avoid confusion later
-- e. g. if SRS considered as TRS, need Epsilon at the right end,
-- otherwise no state of automaton is productive.
-- FIXME: do we need this? 
repair_signature :: TES -> TES
repair_signature trs = 
    let sig = signature trs
        has_nullary = not $ isEmptySet $ sfilter ( (==0) . arity ) sig
    in  trs { signature = if has_nullary
	                  then sig
                          else union sig $ unitSet $ mknullary "eps"
	    }

check_arities :: ( Symbol c , Symbol v )
	      => TRS v c -> TRS v c
check_arities tes = 
    case varying_arities tes of
        [] -> tes
	cas -> error $ show $ vcat
	    [ text "in System" <+> toDoc tes
	    , text ""
	    , text "the following symbols occur with different arities:"
	    , nest 4 $ vcat $ do
	          ( c, occs) <- cas
	          return $ vcat [ text "symbol" <+> toDoc c
				, nest  4 $ vcat ( do
	                   ( a, inf ) <- fmToList occs
	                   return $ hsep [ text "arity", toDoc a, inf ]
		      )
				]
	    ]

varying_arities :: ( Symbol c , ToDoc v )
		=> TRS v c -> [ (c, FiniteMap Int Doc) ]
varying_arities tes = do
    let fm = addListToFM_C plusFM emptyFM $ do
		( k, rule @ Rule {lhs =l, rhs = r} ) <- zip [ 0 :: Int .. ] $ rules tes
		( loc, t ) <- [ ( "lhs", l ), ( "rhs", r ) ]
		let inf = vcat [ fsep [ text "in", text loc
				      , text "of rule number", toDoc k
				      ]
			       , nest 4 $ toDoc rule
			       ]
		c <- symsl t
		return ( c, listToFM [ ( arity c, inf) ] )
    ( c, occs ) <- fmToList fm
    guard $ 1 < sizeFM occs
    return ( c, occs )

-- symbols :: Ord c => [ Rule v c ] -> Set c
symbols rules = unionManySets $ do
    r <- rules
    [ syms $ lhs r , syms $ rhs r ]


