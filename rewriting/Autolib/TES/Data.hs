-- | implements term rewriting systems
-- as represented in the .trs-format

module TES.Data where

--  $Id$

import TES.Symbol
import TES.Term
import TES.Position (syms, vars)
import TES.Rule
import TES.Identifier

import qualified SRS.Rule -- only for instances

import Sets

import ToDoc
import Reader
import Data.List ( partition )

import TES.Sexp
import TES.Parsec

import Letters

data RS t  = RS
	 { annotations :: [ Sexp ]
	 , theory    :: Maybe Sexp
	 , strategy  :: Maybe Sexp
--	 , variables :: Set v -- ^ nullary symbols
--	 , signature :: Set c -- ^ all symbols (not including variables, I hope)
	 , rules :: [ ( t, t ) ]
	 }

type TRS v c = RS ( Term v c )

type TES = TRS Identifier Identifier

type SES = RS [ Identifier ]

instance ( Ord c , Letters t c ) => Letters ( RS t ) c where
    letters rs = unionManySets $ do 
        (l, r) <- rules rs
	return $ letters l `union` letters r

instance ( Ord v , Letters ( Term v c ) v ) => Letters ( TRS v c ) v where
    letters rs = unionManySets $ do 
        (l, r) <- rules rs
	return $ letters l `union` letters r

lhss :: RS t -> [ t ]
lhss trs = do (l,r) <- rules trs ; return l

rhss :: RS t -> [ t ]
rhss trs = do (l,r) <- rules trs ; return r


instance ( ToDoc (t, t), Show (t, t) ) 
	 => ToDoc ( RS t ) where
    toDoc t = vcat [ vcat $ map toDoc $ annotations t 
		   , case theory t of Just x -> toDoc x ; Nothing -> empty
		   , case strategy t of Just x -> toDoc x ; Nothing -> empty
		   -- , toDoc ( wrap "VAR" $ setToList $ variables t )
		   , toDoc ( wrap "RULES" $ rules t )
		   ]

instance ( ToDoc (t, t), Show (t, t) ) 
	 => Show ( RS t ) where show = render . toDoc


instance Read SES where
    readsPrec = parsec_readsPrec

instance Reader SES where
    readerPrec p = do
        plain_reader

instance Read TES where
    readsPrec = parsec_readsPrec

instance Reader TES where
    readerPrec p = do
	tes <- plain_reader
        return $ repair_variables tes

plain_reader :: Reader (t, t) => Parser ( RS t )
plain_reader =  do
        whiteSpace trs
	let trs0 = RS { annotations = []
		     , theory = Nothing
		     , strategy = Nothing
		     -- , variables = emptySet
		     -- , signature = emptySet
		     , rules = []
		     }
        fs <- many line
	return $ foldr (.) id fs trs0

line :: Reader (t, t) =>  Parser ( RS t -> RS t )
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
           }

symbols :: Ord c => [ Rule v c ] -> Set c
symbols rules = unionManySets $ do
    (l, r) <- rules
    [ syms l , syms r ]

signature :: Ord c => TRS v c -> Set c
signature = symbols . rules

{-
at_most_unary :: TRSC v c => TRS v c -> Bool
at_most_unary tes = and $ do
    s <- setToList $ signature tes
    return $ arity s <= 1

has_nullary :: TRSC v c => TRS v c -> Bool
has_nullary tes = or $ do
    s <- setToList $ signature tes
    return $ arity s == 0
-}