module Autolib.Logic.Formula.Name where

import Autolib.ToDoc
import Autolib.Reader
import Data.String

newtype Name = Name String

instance IsString Name where fromString = Name
instance ToDoc Name where toDoc (Name s) = text s
instance Show Name where show = render . toDoc
instance Reader Name where 
    reader = fmap fromString my_identifier
