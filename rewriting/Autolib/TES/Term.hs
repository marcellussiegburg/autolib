module TES.Term 

( module TES.Term
)

where

--   $Id$

import TES.Identifier

import Sets

import ToDoc
import Reader

import Parsec
import ParsecToken
import TES.Parsec

import Util.Size
import Data.FiniteMap

data Term v c = Node c [ Term v c ]
	      | Var v
     deriving ( Eq, Ord )


isvar ( Var _ ) = True
isvar _ = False

unvar ( Var v ) = v

instance ( ToDoc c, ToDoc v ) => ToDoc (Term v c) where
    toDoc ( Var v ) = toDoc v
    toDoc ( Node t xs ) = toDoc t <> 
	if null xs 
	then ToDoc.empty 
	else ToDoc.parens $ hcat $ punctuate ToDoc.comma $ map toDoc xs

instance ( ToDoc c, ToDoc v ) => Show (Term v c) where 
    show = render . toDoc

instance Reader (Term a Identifier ) where
    readerPrec p = do
        t <- readerPrec 0 -- symbol
	xs <- option [] $ ParsecToken.parens tes
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
applyvar :: Ord a => FiniteMap a b -> Term a c -> Term b c
applyvar fm = vmap 
	    ( lookupWithDefaultFM fm (error "TES.Close.applyvar") )

-- | replace variables by terms
-- note the typing: subst must be total on the vars!
apply :: Ord v
      => FiniteMap v ( Term u c ) 
      -> Term v c 
      -> Term u c
apply fm ( Var v ) = lookupWithDefaultFM fm (error "TES.Close.apply") v
apply fm ( Node c args ) = Node c $ map ( apply fm ) args
