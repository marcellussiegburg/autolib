module Util.Bild 

( Bild 
, ToBild (..)
, module Data.Array
)

where

-- -- $Id$

import Data.Array

import ToDoc

type Bild = Array (Int, Int) Char

instance ToDoc Bild where
    toDoc b = vcat $ do
	let ((l,u),(r,o)) = bounds b
	y <- reverse [ u .. o ]
	return $ text $ do
	    x <- [ l ..  r ]
	    return $ b ! (x,y)

instance Show Bild where show = render . toDoc

class ToBild a where
      toBild :: a -> Bild


