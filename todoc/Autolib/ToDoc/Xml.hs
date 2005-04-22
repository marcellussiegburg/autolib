{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}


module Autolib.ToDoc.Xml where

--  $Id$

import Autolib.ToDoc.Class
import Autolib.Xml

instance Container Doc String where
    label _ = "Doc"
    pack = show
    unpack = text
