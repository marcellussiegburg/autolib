{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleContexts #-}

module Data.Autolib.Transport.Class (
    Transport(..),
    Trans(..)
) where

import Data.Autolib.Transport.Atom
import qualified Data.Map as M

data Trans atom
    = Atom atom
    | Array [Trans atom]
    | Object (M.Map String (Trans atom))

class Atom atom => Transport base atom | base -> atom where
    encode :: Trans atom -> base
    decode :: base -> Maybe (Trans atom)
