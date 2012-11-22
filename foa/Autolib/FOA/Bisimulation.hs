-- | this paper mentions the bisimulation idea:
-- Experiments with Deterministic ω-Automata
-- for Formulas of Linear Temporal Logic
-- Joachim Klein and Christel Baier
-- http://www.inf.tu-dresden.de/content/institutes/thi/algi/publikationen/texte/11_05.pdf
-- we start with 0-equivalence: this is an equivalence
-- that is compatible with the acceptance condition.
-- Then we refine: two states are 
-- equivalent if each path of length k
-- leads to a pair of 0-equivalent states.
-- then we make k large enough.

module Autolib.FOA.Bisimulation where

