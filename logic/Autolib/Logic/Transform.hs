-- | from MSO to MSO_0, 
-- as in W. Thomas: Languages, Automata, and Logic, in: HFL, p. 395

module Autolib.Logic.Transform where

import Autolib.Logic.Formula

-- | input: arbitrary; 
-- output: equivalent formula without FO variables.
-- implementation: each FO variable is replaced by a singleton SO variable.
mso2mso0 :: Formula FOName SOName -> Formula Void SOName
mso2mso0 f = case f of

    SuccFO (Unlift l) (Unlift r) -> SuccSO l r
    Apply s (Unlift u) -> Subseteq u s

    Or l r -> Or (mso2mso0 l) (mso2mso0 r)
    And l r -> And (mso2mso0 l) (mso2mso0 r)
    Implies l r -> Implies (mso2mso0 l) (mso2mso0 r)

    ForallFO fun -> ForallSO $ \ n -> Implies (Singleton n) (mso2mso0 $ fun $ Unlift n )
    ExistsFO fun -> ExistsSO $ \ n -> And (Singleton n) (mso2mso0 $ fun $ Unlift n )
    ForallSO fun -> ForallSO $ \ n -> mso2mso0 $ fun n
    ExistsSO fun -> ExistsSO $ \ n -> mso2mso0 $ fun n

    _ -> error $ "mso2mso0: " ++ show f
