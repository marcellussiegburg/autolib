-- | design question: what to do with variables?
-- should variable positions be included in addresses?

module Autolib.Rewriting.Terms where

--  $Id$

import Autolib.Rewriting.Address
import Autolib.TES.Term
import qualified Autolib.TES.Position as T

instance Sub ( Term v ) where
    -- top (leftmost) symbol
    -- top :: c a -> a
    top ( Node f _ ) = f
    top ( Var _ ) = error "top: Variable does not contain symbol"

    children ( Node f args ) = args
    children ( Var v )       = []
    build f args = Node f args

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


