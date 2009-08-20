-- -*- mode: haskell -*-

module Autolib.Reader.Derived where

--   $Id$

import Autolib.Reader.Class
import Autolib.Reader.Basic

{-! for Bool   derive : Reader !-}
{-! for Maybe  derive : Reader !-}
{-! for Either derive : Reader !-}

