module TES.Term 

( module TES.Term
)

where

--   $Id$

import TES.Symbol
import TES.Identifier

import Sets

import ToDoc
import Reader

import TES.Parsec
import Text.ParserCombinators.Parsec.Combinator (option)

import Util.Size
import Data.FiniteMap

data Term v c = Node c [ Term v c ]
	      | Var v
     deriving ( Eq, Ord )

top :: Term v c -> c
top (Node c args ) = c 

children :: Term v c -> [ Term v c ]
children (Node c args) = args

class ( Show v, Show c, Ord v, ToDoc v, ToDoc [v], Symbol c ) 
      => TRSC v c -- no methods
instance ( Show v, Show c, Ord v, ToDoc v, ToDoc [v], Symbol c ) 
      => TRSC v c 


isvar ( Var _ ) = True
isvar _ = False

unvar ( Var v ) = v

instance ( ToDoc c, ToDoc v ) => ToDoc (Term v c) where
    toDoc ( Var v ) = toDoc v
    toDoc ( Node t xs ) = toDoc t <> 
	if null xs 
	then ToDoc.empty 
	else ( text " " <+> ) 
    	     $ ToDoc.parens $ hcat $ punctuate ( text ", " )
			    $ map toDoc xs

instance ( ToDoc c, ToDoc v ) => Show (Term v c) where 
    show = render . toDoc

instance Reader (Term a Identifier ) where
    readerPrec p = do
        t <- readerPrec 0 -- symbol
	xs <- option [] $ TES.Parsec.parens tes
		        $ commaSep tes 
	                $ readerPrec 0 
	return $ Node ( t { i_arity = length xs } ) xs

instance Read (Term a Identifier) where
    readsPrec = parsec_readsPrec


instance Functor (Term v) where
    fmap f (Var v) = Var v
    fmap f (Node c args) = Node (f c) $ map (fmap f) args

vmap ::  (v -> w) -> ( Term v c -> Term w c )
vmap f (Var v) = Var (f v)
vmap f (Node c args) = Node c $ map (vmap f) args

----------------------------------------------------------


-- | replace variables by variables
applyvar :: ( TRSC a c, TRSC b c )
	 => FiniteMap a b 
         -> Term a c -> Term b c
applyvar fm t = vmap 
	    ( lookupWithDefaultFM fm 
	      ( error $ "TES.Term.applyvar" ++ show fm ++ " to " ++  show t )
	    ) t

-- | replace variables by terms
-- note the typing: subst must be total on the vars!
apply :: Ord v
      => FiniteMap v ( Term u c ) 
      -> Term v c 
      -> Term u c
apply fm ( Var v ) = lookupWithDefaultFM fm (error "TES.Term.apply") v
apply fm ( Node c args ) = Node c $ map ( apply fm ) args

