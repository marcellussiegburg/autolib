module Autolib.Logic.NFA where

import Autolib.Logic.Formula

import Autolib.NFA

-- | input: MSO_0 formula,
-- output: equivalent automaton

to_nfa :: Formula -> NFA ( SOName, [ Bool ] ) Int
to_nfa f = undefined
