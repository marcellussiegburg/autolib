-- | design question: what to do with variables?
-- should variable positions be included in addresses?

module Rewriting.Terms where

--  $Id$

import Rewriting.Address
import TES.Term
import qualified TES.Position as T

instance Sub ( Term v ) where
    -- top (leftmost) symbol
    -- top :: c a -> a
    top ( Node f _ ) = f
    top ( Var _ ) = error "top: Variable does not contain symbol"

    -- replace top symbol
    -- replace :: c a -> a -> c a
    replace ( Node _ args ) f = Node f args
    replace ( Var _ ) f = error "replace: cannot replace Variable"

    substructures = T.subterms -- with variables ??
    symbols = T.symsl -- no variables

instance Is_Top [ Int ] where
    is_top = null


instance Address ( Term v ) [ Int ] where

    -- all addresses
    -- addresses :: c a -> [ p ]
    addresses = T.pos 

    -- get substructure
    -- peek :: c a -> p -> c a
    peek  = T.peek

    -- replace substructure
    -- poke :: c a -> ( p, c a ) -> c a
    poke = T.poke

    -- compute new symbol at position
    -- pmap :: ( p -> a -> b ) -> c a -> c b
    pmap = T.pmap


