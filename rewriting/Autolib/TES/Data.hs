{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

-- | implements term rewriting systems
-- as represented in the .trs-format

module Autolib.TES.Data where

--  $Id$

import Autolib.Symbol
import Autolib.TES.Term
import Autolib.TES.Position (syms, vars)
import Autolib.TES.Rule
import Autolib.TES.Identifier

import qualified Autolib.SRS.Rule -- only for instances

import Autolib.Sets

import Autolib.ToDoc
import Autolib.Reader
import Data.List ( partition )
import Data.FiniteMap
import Control.Monad ( guard )

import Autolib.TES.Sexp
import Autolib.TES.Parsec
import Autolib.TES.Position (symsl)

import Autolib.Letters

data RS c t  = RS
	 { annotations :: [ Sexp ]
	 , theory    :: Maybe [ Sexp ]
	 , strategy  :: Maybe [ Sexp ]

--	 , variables :: Set v -- ^ nullary symbols
	 , signature :: Set c -- ^ all symbols (not including variables, I hope)
	 , separate :: Bool
	 , rules :: [ ( t, t ) ]
	 }
    deriving ( Eq, Ord )


from_rules :: Bool -> [ ( t,t ) ] -> RS c t
from_rules sep rs = RS { annotations = []
		   , theory = Nothing
		   , strategy = Nothing
		   , signature = error "TES.Data.from_rules"
		   , separate = sep
		   , rules = rs
		   }

from_srs = from_rules True

type TRS v c = RS c ( Term v c )

type TES = TRS Identifier Identifier

type SES = RS Identifier [ Identifier ]

instance ( Ord c , Letters t c ) => Letters ( RS c t ) c where
    letters rs = unionManySets $ do 
        (l, r) <- rules rs
	return $ letters l `union` letters r

{-
instance ( Ord v , Letters ( Term v c ) v ) => Letters ( TRS v c ) v where    letters  = variables
-}

variables rs =  unionManySets $ do 
        (l, r) <- rules rs
	return $ vars l `union` vars r

lhss :: RS c t -> [ t ]
lhss trs = do (l,r) <- rules trs ; return l

rhss :: RS c t -> [ t ]
rhss trs = do (l,r) <- rules trs ; return r


instance ( ToDoc (t, t), Show (t, t) ) 
	 => ToDoc ( RS c t ) where
    toDoc t = vcat [ vcat $ map toDoc $ annotations t 
		   , case theory t of 
			  Just x -> toDoc $ List $ Leaf "THEORY"   :  x 
			  Nothing -> empty
		   , case strategy t of 
		          Just x -> toDoc $ List $ Leaf "STRATEGY" :  x 
			  Nothing -> empty
		   -- , toDoc ( wrap "VAR" $ setToList $ variables t )
		   , toDoc $ ( if separate t then wrap_sep "," else wrap )
                             "RULES" $ rules t 
		   ]

instance ( ToDoc (t, t), Show (t, t) ) 
	 => Show ( RS c t ) where show = render . toDoc


instance Read SES where
    readsPrec = parsec_readsPrec

instance Reader SES where
    readerPrec p = do
        ses <- plain_reader
	return $ ses { separate = True }

instance Read TES where
    readsPrec = parsec_readsPrec

instance Reader TES where
    readerPrec p = do
	tes <- plain_reader
        return $ check_arities
	       $ repair_signature
	       $ repair_variables 
	       $ tes { separate = False }

plain_reader :: Reader (t, t) => Parser ( RS c t )
plain_reader =  do
        whiteSpace trs
	let trs0 = RS { annotations = []
		     , theory = Nothing
		     , strategy = Nothing
		     -- , variables = emptySet
		     , signature = emptySet
		     , rules = []
		     , separate = False
		     }
        fs <- many line
	return $ foldr (.) id fs trs0

line :: Reader (t, t) =>  Parser ( RS c t -> RS c t )
line = Autolib.TES.Parsec.parens Autolib.TES.Parsec.trs $  do
     f <- identifier  Autolib.TES.Parsec.trs 
     case f of
	  "RULES" -> do
	      rs <- many reader
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
    let vhead (List (Leaf "VAR" : _ )) = True ; vhead _ = False
        vs = mkSet $ do 
		List ( Leaf "VAR" : xs ) <- annotations trs
		Leaf x <- xs
	        return $ mknullary x
	-- change (some) nullary ids to vars
	xform ( Node c [] ) | c `elementOf` vs = Var c
	xform ( Node c args ) = Node c ( map xform args )
	-- apply to rules
        rs = do (l,r) <- rules trs ; return ( xform l, xform r )
	sig = sfilter ( \ s -> not (s `elementOf` vs)) $ symbols rs
    in  trs { rules = rs
	    , signature = sig
           }

-- | add nullary symbol (if not already there) to avoid confusion later
-- e. g. if SRS considered as TRS, need Epsilon at the right end,
-- otherwise no state of automaton is productive
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
		( k, rule @ ( l, r ) ) <- zip [ 0 :: Int .. ] $ rules tes
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

symbols :: Ord c => [ Rule v c ] -> Set c
symbols rules = unionManySets $ do
    (l, r) <- rules
    [ syms l , syms r ]


