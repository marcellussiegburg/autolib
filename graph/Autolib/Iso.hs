module Iso where

-- | Klasse fuer den Isomorphietest
class Iso a where
  iso :: a -> a -> Bool
