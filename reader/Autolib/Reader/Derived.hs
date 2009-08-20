-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, OverlappingInstances #-}

module Autolib.Reader.Derived where

--   $Id$

import Autolib.Reader.Class
import Autolib.Reader.Basic
import Data.Derive.Reader

$(derives [makeReader] [''Bool, ''Maybe, ''Either])

