-- | from MSO to MSO_0, 
-- as in W. Thomas: Languages, Automata, and Logic, in: HFL, p. 395

module Autolib.Logic.Transform where

import qualified Autolib.Logic.Formula.FO as FO
import qualified Autolib.Logic.Formula.SO as SO

s1 :: SO.Formula
s1 = fo2mso0 FO.f1

-- | input: arbitrary; 
-- output: equivalent formula without FO variables.
-- implementation: each FO variable is replaced by a singleton SO variable.
fo2mso0 :: FO.Formula -> SO.Formula 
fo2mso0 (FO.Formula f) = convert f

convert :: FO.Form Int -> SO.Formula
convert f = case f of
    FO.Succ l r -> SO.Succ l r
    FO.Less l r -> SO.Less l r
    FO.Letter c a -> SO.Letter c a

    FO.Or l r -> SO.Or (convert l) (convert r)
    FO.And l r -> SO.And (convert l) (convert r)
    FO.Implies l r -> SO.Implies (convert l) (convert r)

    FO.Forall fun -> SO.Forall $ \ n -> 
        SO.Implies (SO.Singleton n) (convert $ fun  n )
    FO.Exists fun -> SO.Exists $ \ n -> 
        SO.And (SO.Singleton n) (convert $ fun  n )
