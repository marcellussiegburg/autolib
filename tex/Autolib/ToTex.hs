module ToTex 

-- $Id$

( module ToTex
, module Tex
)

where

import Tex

class Show a => ToTex a where 
    toTex :: a -> Tex
    toTex = Direct . show  -- default

instance ToTex Int




