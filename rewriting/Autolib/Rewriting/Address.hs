module Autolib.Rewriting.Address where

--  $Id$

class Functor c => Sub c where
    -- | top (leftmost) symbol
    top :: c a -> a

    -- | immediate (left-to-right)
    children :: c a -> [ c a ]
    
    -- |  with the idea that @t = build (top t) (children t)@
    build :: a -> [ c a ] -> c a 

    -- | replace top symbol
    replace :: c a -> a -> c a

    -- | all of them
    substructures :: c a -> [ c a ]

    symbols :: c a -> [ a ]
    symbols t = do s <- substructures t ; return $ top t


class Is_Top p where
    is_top :: p -> Bool


class ( Sub c, Is_Top p ) => Address c p | c -> p where

    addresses :: c a -> [ p ]

    -- | get substructure
    peek :: c a -> p -> c a
    -- | replace substructure
    poke :: c a -> ( p, c a ) -> c a

    -- | compute new symbol at position
    pmap :: ( p -> a -> b ) -> c a -> c b



peek_symbol :: Address c p 
	    => c a -> p -> a
peek_symbol t p = top ( peek t p )

-- | implementation is inefficient
poke_symbol :: Address c p 
	    => c a -> (p, a) -> c a
poke_symbol t (p, x) = poke t ( p, replace ( peek t p ) x )
