module TES.Draw 

( draw )

where

--  $Id$

import TES.Type
import qualified Data.Tree as D
import Autolib.ToDoc

data Container = Container String
instance Show Container where
    show ( Container c ) = c

make :: ( Show v, Show c )
     => Term v c 
     -> D.Tree Container
make = tfold ( \ v -> D.Node ( Container $ show v ) [] )
             ( \ c xs -> D.Node ( Container $ show c ) $ reverse xs )

draw :: ( Show v, Show c )
     => Term v c
     -> Doc
draw = vcat
     . map text
     . lines
     . D.drawTree
     . make



