module Exp.Simplify where

-- -- $Id$

import Exp.Type

-- (slightly) simplifying ersatz constructors
-- assumes standard interpretation of some Refs

dot (Ref "Empty") _ = Ref "Empty"
dot _ (Ref "Empty") = Ref "Empty"
dot (Ref "Eps") l   = l
dot l (Ref "Eps")   = l
dot x y = Dot x y

union (Ref "Empty") l = l
union l (Ref "Empty") = l
union x y = if x == y then x else Union x y

star (Ref "Empty") = Ref "Eps"
star (Ref "Eps")   = Ref "Eps"
star l             = Star l
