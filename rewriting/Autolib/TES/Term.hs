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

subterms :: Term v c -> [ Term v c ]
subterms t = t : case t of
    Var v -> []
    Node c args -> do arg <- args ; subterms arg

instance Size (Term v c) where
    size t = length $ do
	 Node _ _ <- subterms t
	 return ()

syms :: Ord c => Term v c -> Set c
syms t = mkSet $ do
    Node c _ <- subterms t
    return c

vars :: Ord v => Term v c -> Set v
vars t = mkSet $ do
    Var v <- subterms t
    return v

isvar ( Var _ ) = True
isvar _ = False

unvar ( Var v ) = v

-- | replace variables by variables
apply :: Ord a => FiniteMap a b -> Term a c -> Term b c
apply fm = vmap ( lookupWithDefaultFM fm (error "TES.Close.apply") )
