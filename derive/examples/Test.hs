{-# LANGUAGE TemplateHaskell, EmptyDataDecls #-}
import Data.Derive.Reader
import Data.Derive.ToDoc

import Autolib.Reader

data MMaybe a = JJust a | NNothing

data Frotz = A | B Frotz | C { sub :: Frotz } | D { sub :: Frotz, foo :: Frotz }

$(deriveReader ''MMaybe)
$(deriveReader ''Frotz)
