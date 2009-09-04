{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
    FlexibleContexts #-}

module Data.Autolib.Transport.Class (
    Transport(..),
    Trans(..),
    ToTransport(..)
) where

import Data.Autolib.Transport.Error
import Data.Autolib.Transport.Atom
import qualified Data.Map as M

data Trans atom
    = TrAtom atom
    | TrArray [Trans atom]
    | TrObject (M.Map String (Trans atom))

class Atom atom => Transport base atom | base -> atom where
    encode :: Trans atom -> base
    decode :: base -> Error (Trans atom)

class ToTransport a where
    toTransport :: Atom atom => a -> Trans atom
    fromTransport :: Atom atom => Trans atom -> Error a
