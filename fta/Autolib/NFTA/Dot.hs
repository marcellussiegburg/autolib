module NFTA.Dot where

--  $Id$

import NFTA.Type
import qualified NFTA.Path
import qualified NFA.Dot
import Dot.Dot

instance ( Show s, Show c , NFTAC c s ) => ToDot (NFTA c s) where
    toDot = toDot . NFTA.Path.make
    toDotProgram a = "dot"
    toDotOptions a = "-Grankdir=LR"


