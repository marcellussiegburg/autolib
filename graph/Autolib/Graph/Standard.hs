module Graph.Standard where

-- $Id$

import Graph.Graph 
import Graph.Basic
import Graph.Ops
import Graph.Display

import Sets
import Util.Teilfolgen
import Monad ( guard )
import ToDoc

star :: Int -> Graph Int
star n = times0 ( independent $ mkSet [ 0 ]    )
	       ( independent $ mkSet [ 1 .. n ] )

wheel :: Int -> Graph Int
wheel n = times0 ( independent $ mkSet [ 0 ]    )
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
      $ foldr1 ( \ x y -> normalize $ times x y ) 
      $ do n <- ns ; return $ e n

herschel :: Graph Int
herschel = informed ( text "Herschel" )
	 $ normalize
	 $ Graph.Ops.union ( grid (path [1..3]) (path [1 .. 3]) )
                 ( independent $ mkSet [1 , 3 ] )
	   `links0` do 
		v <- [1, 3 ]
		let other v = 4 - v
		[ kante (Left (v,v)) (Right v)
		   , kante (Left (v,v)) (Right $ other v)
		   , kante (Left (v, other v)) (Right v)
		   ]

