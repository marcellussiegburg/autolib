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
import Text.ParserCombinators.Parsec.Expr

import Util.Size
import Data.FiniteMap
import Data.Maybe

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

instance ( Symbol c, ToDoc v ) => ToDoc (Term v c) where
    toDocPrec _ ( Var v ) = toDoc v
    toDocPrec p ( Node t xs ) = 
	case ( arity t, precedence t ) of
             ( 2, Just q ) -> ( if p > q then ToDoc.parens else id )
			    $ fsep [ toDocPrec q ( xs !! 0)
				   , toDoc t <+> toDocPrec q ( xs !! 1)
				   ]
	     ( 1, Just q ) -> ( if p > q then ToDoc.parens else id )
			    $ toDoc t <+> toDocPrec q ( xs !! 0)
	     _ -> toDoc t <+> 
		if null xs 
		then ToDoc.empty 
		else ToDoc.parens $ hcat $ punctuate ( text ", " )
			    $ map toDoc xs

instance ( Symbol c, ToDoc v ) => Show (Term v c) where 
    show = render . toDoc

instance ( ToDoc v, Symbol c, ToTex v, ToTex c ) 
         => ToTex ( Term v c ) where
    toTex ( Var v ) = toTex v
    toTex ( Node c [] ) = toTex c
    toTex ( Node c [l, r] ) = 
        Macro "tree" [ Opt $ toTex c, Req $ toTex l, Req $ toTex r ]


treader :: ( Symbol c, Read v )
	    => [c] -- ^ predefined symbols (with precedences)
	    -> Bool -- ^ True : parse unknown as symbol
		    --  False : parse unknown as variable 
	    -> Parser ( Term v c )
treader ops flag = do
    buildExpressionParser (operators ops) (atomic ops flag)


operators :: ( Symbol c , Read v )
	  => [c]
	  -> OperatorTable Char () ( Term v c )
operators ops  =
    do ops <- reverse $ collectBy precedence 
		      $ filter ( isJust  . precedence )
		      $ ops
       return $ do op <- ops 
		   case arity op of
		       1 -> return $ Prefix ( do 
				     symbol trs (show op)
				     return $ \ x -> Node op [x] )
		       2 -> return $ Infix ( do 
				     symbol trs (show op)
				     return $ \ x y -> Node op [x,y] ) 
				  AssocLeft
		       _ -> []

collectBy :: Ord b => (a -> b) -> [a] -> [[a]]
-- from lowest to highest
collectBy f xs = 
    eltsFM $ addListToFM_C (++) emptyFM $ do
        x <- xs ; return (f x, [x])

atomic :: ( Symbol c, Read v )
       => [ c ]
       -> Bool
       -> Parser (Term v c)
atomic ops flag = 
      TES.Parsec.parens trs ( treader ops flag )
  <|> do
        t <- readerPrec 0 -- symbol
	mxs <- option Nothing 
			$ fmap Just
                        $ TES.Parsec.parens trs
		        $ commaSep trs
	                $ treader ops flag
        return $ case mxs of
	     Nothing -> case flag of
			     True  -> Node ( set_arity 0 t ) []
			     False -> Var  ( read $ show t )
	     Just xs -> Node ( set_arity (length xs) t ) xs

instance Reader (Term Identifier Identifier ) where
    readerPrec p =  treader [] True

instance Read (Term Identifier Identifier) where
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

