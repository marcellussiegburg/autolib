{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}

-- | Terms

module Autolib.TES.Type 

( module Autolib.TES.Type
, module Autolib.TES.Raw
)

where

--  $Id$

import Autolib.Util.Size
import Autolib.Symbol
import Autolib.TES.Raw
import Autolib.TES.Xml

import Text.XML.HaXml.Haskell2Xml
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash



class ( Hash v,  Show v, Show c
      , Ord v, ToDoc v, ToDoc [v]
      , Reader v
      , Symbol c ) 
      => TRSC v c -- no methods
instance ( Hash v, Show v, Show c
	 , Ord v, ToDoc v, ToDoc [v]
	 , Reader v
	 , Symbol c ) 
      => TRSC v c 

instance TRSC v c => Hash ( Term v c ) where
    hash (Node f args) = hash (f, args)
    hash (Var v) = hash (666 :: Int, v)

-- | root symbol
top :: Term v c -> c
top (Node c args ) = c 

children :: Term v c -> [ Term v c ]
children (Node c args) = args

instance Functor (Term v) where
    fmap f (Var v) = Var v
    fmap f (Node c args) = Node (f c) $ map (fmap f) args

-- | apply mapping to variables only
vmap ::  (v -> w) -> ( Term v c -> Term w c )
vmap f (Var v) = Var (f v)
vmap f (Node c args) = Node c $ map (vmap f) args


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

-- | apply to unary symbols only
nullmap :: ( c -> Term v c ) 
	-> Term v c 
	-> Term v c
nullmap f =
    let fnode c [] = f c
	fnode c args = Node c args
    in  tfold Var fnode
