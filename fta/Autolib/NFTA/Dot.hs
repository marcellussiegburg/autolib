module Autolib.NFTA.Dot where

--  $Id$

import Autolib.NFTA.Type
import qualified Autolib.NFTA.Path
import qualified Autolib.NFA.Dot
import Autolib.Dot.Dot

instance ( Show s, Show c , NFTAC c s ) => ToDot (NFTA c s) where
    toDot = toDot . Autolib.NFTA.Path.make
    toDotProgram a = "dot"
    toDotOptions a = "-Grankdir=LR"


