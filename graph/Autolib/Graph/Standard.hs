module Graph.Standard where

-- -- $Id$

import Graph.Type
import Graph.Basic
import qualified Graph.Ops
import Graph.Display

import Sets 
import Util.Teilfolgen
import Monad ( guard )
import ToDoc hiding ( empty )

star :: Int -> Graph Int
star n = Graph.Ops.times0 ( independent $ mkSet [ 0 ]    )
	       ( independent $ mkSet [ 1 .. n ] )

wheel :: Int -> Graph Int
wheel n = Graph.Ops.times0 ( independent $ mkSet [ 0 ]    )
	       ( circle [ 1 .. n ] )

c :: Int -> Graph Int
c n = circle [ 1 .. n ]

p :: Int -> Graph Int
p n = path [ 1 .. n ]

e :: Int -> Graph Int
e n = independent $ mkSet [ 1 .. n ]

k :: Int -> Graph Int
k n = clique $ mkSet [ 1 .. n ]

kk :: [ Int ] -> Graph Int
kk ns = informed ( funni "K" $ map toDoc ns )
      $ foldr1 ( \ x y -> Graph.Ops.normalize $ Graph.Ops.times x y ) 
      $ do n <- ns ; return $ e n


