module TES.Position where

--  $Id$

import TES.Term
import Util.Size
import Sets

type Position = [ Int ]

positions :: Term v c 
          -> [ ( Position, Term v c ) ]
positions t = ( [], t ) : case t of
    Node c args -> do ( k, arg ) <- zip [ 0 .. ] args
		      ( p, s   ) <- positions arg
		      return ( k : p , s )
    _ -> []

pos :: Term v c 
    -> [ Position ]
pos t = do
    ( p, s ) <- positions t
    return p

subterms :: Term v c 
	 -> [ Term v c ]
subterms t = do
    ( p, s ) <- positions t
    return s

peek :: Term v c 
     -> Position 
     -> Term v c
peek t [] = t
peek ( Node c args ) ( k : ks ) = peek ( args !! k ) ks

peek_symbol :: Term v c 
     -> Position 
     -> c
peek_symbol t p = 
    case peek t p of
         Node c args -> c
	 _ -> error "TES.Position.peek_symbol called for non-symbol"

-- | warning: don't check arity
poke_symbol ::  Term v c 
     -> ( Position , c )
     -> Term v c
poke_symbol t ( p, c ) =  
    case peek t p of
         Node _ args -> poke t ( p, Node c args )
	 _ -> error "TES.Position.poke_symbol called for non-symbol"

poke :: Term v c 
     -> ( Position , Term v c )
     -> Term v c
poke t ( [], s ) = s
poke (Node c args) (k : ks, s ) = 
    let ( pre , this : post ) = splitAt k args
    in Node c ( pre ++ poke this ( ks, s ) : post )

pokes :: Term v c
      -> [ ( Position, Term v c ) ]
      -> Term v c
pokes = foldl poke



instance Size (Term v c) where
    size t = length $ do
	 Node _ _ <- subterms t
	 return ()

-- | size w each constant subtree replaced by one node
vsize :: Term v c -> Int
vsize = size . smash


syms :: Ord c => Term v c -> Set c
syms t = mkSet $ do
    Node c _ <- subterms t
    return c

lsyms :: Ord c => Term v c -> [ c ]
lsyms = setToList . syms

vars :: Ord v => Term v c -> Set v
vars t = mkSet $ do
    Var v <- subterms t
    return v

-- | list of variables (each occurs once, unspecified ordering)
lvars :: Ord v => Term v c -> [ v ]
lvars = setToList . vars

-- | list of variables (in pre-order, with duplicates)
voccs :: Ord v => Term v c -> [ v ]
voccs t = do ( p, Var v ) <- positions t ; return v
