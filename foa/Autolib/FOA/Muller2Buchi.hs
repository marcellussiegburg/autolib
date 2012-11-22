-- | from non-deterministic Muller 
-- to non-deterministic Buchi.
-- see Lemma 5 in  K. Narayan Kumar
-- http://www.cmi.ac.in/~kumar/words/lecture08.pdf

module Autolib.FOA.Muller2Buchi where

import Autolib.FOA.Data

m2b a | Muller m <- acceptance a =
    