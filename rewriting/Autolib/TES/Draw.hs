module Autolib.TES.Draw 

( draw )

where

--  $Id$

import Autolib.TES.Type
import qualified Data.Tree as D
import Autolib.ToDoc

data Container = Container String
instance Show Container where
    show ( Container c ) = c

pack x = 
#if (__GLASGOW_HASKELL__ < 604)
    Container
#endif
        ( show x )

make :: ( Show v, Show c )
     => Term v c 
     -> D.Tree 
#if (__GLASGOW_HASKELL__ < 604)
               Container
#else 
               String
#endif
make = tfold ( \ v -> D.Node ( pack v ) [] )
             ( \ c xs -> D.Node ( pack c ) $ reverse xs )

draw :: ( Show v, Show c )
     => Term v c
     -> Doc
draw = vcat
     . map text
     . lines
     . D.drawTree
     . make



