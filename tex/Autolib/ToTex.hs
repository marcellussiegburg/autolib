module Autolib.ToTex 

--  $Id$

( module Autolib.ToTex
, module Autolib.Tex
)

where

import Autolib.Tex

class Show a => ToTex a where 
    toTex :: a -> Tex
    toTex = Direct . show  -- default

instance ToTex Int




