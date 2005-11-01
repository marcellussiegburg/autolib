module Autolib.ToTex 

--  $Id$

( module Autolib.ToTex
, module Autolib.Tex
)

where

import Autolib.Tex

class ToTex a where 
    toTex :: a -> Tex

instance Show a => ToTex a where
    toTex = Direct . show  -- default






