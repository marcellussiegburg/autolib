-- | implements term rewriting systems
-- as represented in the .tes-format

module TES.Data where

--  $Id$

import TES.Symbol
import TES.Term
import TES.Position (syms, vars)
import TES.Rule
import TES.Identifier

import Sets

import ToDoc
import Reader

import TES.Sexp
import TES.Parsec


data TRS v c  = TRS
	 { annotations :: [ Sexp ]
	 , theory    :: Maybe Sexp
	 , strategy  :: Maybe Sexp
	 , variables :: Set v -- ^ nullary symbols
	 , signature :: Set c -- ^ all symbols (not including variables, I hope)
	 , rules :: [ Rule v c ]
	 }

lhss :: TRS v c -> [ Term v c ]
lhss trs = do (l,r) <- rules trs ; return l

rhss :: TRS v c -> [ Term v c ]
rhss trs = do (l,r) <- rules trs ; return r

type TES = TRS Identifier Identifier

instance ( TRSC v c ) 
	 => ToDoc ( TRS v c ) where
    toDoc t = vcat [ vcat $ map toDoc $ annotations t 
		   , case theory t of Just x -> toDoc x ; Nothing -> empty
		   , case strategy t of Just x -> toDoc x ; Nothing -> empty
		   , toDoc ( wrap "VAR" $ setToList $ variables t )
		   , toDoc ( wrap "RULES" $ rules t )
		   ]

instance ( TRSC v c )
	 => Show ( TRS v c ) where show = render . toDoc

instance Read TES where
    readsPrec = parsec_readsPrec

instance Reader TES where
    readerPrec p = do
        whiteSpace trs
	let trs0 = TRS { annotations = []
		     , theory = Nothing
		     , strategy = Nothing
		     , variables = emptySet
		     , signature = emptySet
		     , rules = []
		     }
        fs <- many line
	let t = foldr (.) id fs trs0
	return $ repair_variables t

line :: Parser ( TES -> TES )
line = TES.Parsec.parens TES.Parsec.trs $  do
     f <- identifier  TES.Parsec.trs 
     case f of
	  "RULES" -> do
	      rs <- many reader
	      return $ \ t -> t { rules = rules t ++ rs }
	  _ -> do
              args <- many reader
	      return $ \ t -> t { annotations = annotations t 
			            ++ [ List $ Leaf f  : args ]
				}

repair_variables :: TES -> TES
repair_variables trs =
    let vars = mkSet $ do 
		List ( Leaf "VAR" : vs ) <- annotations trs
		Leaf v <- vs
	        return $ mknullary v
	-- change (some) nullary ids to vars
	xform ( Node c [] ) | c `elementOf` vars = Var c
	xform ( Node c args ) = Node c ( map xform args )
	-- apply to rules
        rs = do (l,r) <- rules trs ; return ( xform l, xform r )
	sig = sfilter ( \ s -> not (s `elementOf` vars)) $ symbols rs
    in  trs { variables = vars
	    , signature = sig
	    , rules = rs
            }

symbols :: Ord c => [ Rule v c ] -> Set c
symbols rules = unionManySets $ do
    (l, r) <- rules
    [ syms l , syms r ]

at_most_unary :: TRSC v c => TRS v c -> Bool
at_most_unary tes = and $ do
    s <- setToList $ signature tes
    return $ arity s <= 1

has_nullary :: TRSC v c => TRS v c -> Bool
has_nullary tes = or $ do
    s <- setToList $ signature tes
    return $ arity s == 0
