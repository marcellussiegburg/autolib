module TES.Type where

--  $Id$

import Util.Size
import TES.Symbol
import ToDoc

data Term v c = Node c [ Term v c ]
	      | Var v
     deriving ( Eq, Ord )

class ( Show v, Show c, Ord v, ToDoc v, ToDoc [v], Symbol c ) 
      => TRSC v c -- no methods
instance ( Show v, Show c, Ord v, ToDoc v, ToDoc [v], Symbol c ) 
      => TRSC v c 

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

