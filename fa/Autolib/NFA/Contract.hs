module NFA.Contract where

-- $Id$

import NFA.Type

contract :: NFAC c a
         => NFA c a -> (a, a) -> NFA c a
-- identify states p and q
-- creating an automaton that accepts a superset
contract aut (p, q) =
    let f r = if r == q then p else r
    in  statemap f aut

