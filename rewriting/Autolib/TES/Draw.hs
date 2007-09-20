module Autolib.TES.Draw 

( draw )

where

--  $Id$

import Autolib.TES.Type
import qualified Data.Tree as D
import Autolib.ToDoc

make :: ( Show v, Show c )
     => Term v c 
     -> D.Tree 
               String

make = tfold ( \ v -> D.Node ( show v ) [] )
             ( \ c xs -> D.Node ( show c ) $ reverse xs )

draw :: ( Show v, Show c )
     => Term v c
     -> Doc
draw = vcat
     . map text
     . lines
     . D.drawTree
     . make



