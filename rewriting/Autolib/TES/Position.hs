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

lvars :: Ord v => Term v c -> [ v ]
lvars = setToList . vars