module TES.Term 

( module TES.Term
)

where

--   $Id$

import TES.Symbol
import TES.Identifier

import Sets

import ToDoc
import ToTex
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


isvar :: Term v c -> Bool
isvar ( Var _ ) = True
isvar _ = False

unvar :: Term v c -> v
unvar ( Var v ) = v

-- | replace each const subtree by Node ()
smash :: Term v c -> Term v Bool
smash = tfold ( \ v -> Var v )
	      ( \ f args -> if    and ( map ( not . isvar ) args )
			       && and ( map top args ) -- all const
			    then Node True []
			    else Node False args
	      )

tfold :: ( v -> a )
      -> ( c -> [a] -> a )
      -> Term v c 
      -> a
tfold fvar fnode = fun where
    fun ( Var v ) = fvar v
    fun ( Node f args ) = fnode f ( map fun args )

instance ( ToDoc c, ToDoc v ) => ToDoc (Term v c) where
    toDoc ( Var v ) = ToDoc.brackets $ toDoc v
    toDoc ( Node t xs ) = toDoc t <+> 
	if null xs 
	then ToDoc.empty 
	else ToDoc.parens $ hcat $ punctuate ( text ", " )
			    $ map toDoc xs

instance ( ToDoc c, ToDoc v ) => Show (Term v c) where 
    show = render . toDoc

instance ( ToDoc v, ToDoc c, ToTex v, ToTex c ) 
         => ToTex ( Term v c ) where
    toTex ( Var v ) = toTex v
    toTex ( Node c [] ) = toTex c
    toTex ( Node c [l, r] ) = 
        Macro "tree" [ Opt $ toTex c, Req $ toTex l, Req $ toTex r ]

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


type Substitution v c = FiniteMap v ( Term v c )

-- | replace variables by terms
-- here, fm may be partial
apply_partial :: Ord v
      => Substitution v c
      -> Term v c 
      -> Term v c
apply_partial fm t @ ( Var v ) = lookupWithDefaultFM fm t v
apply_partial fm ( Node c args ) = Node c $ map ( apply_partial fm ) args

