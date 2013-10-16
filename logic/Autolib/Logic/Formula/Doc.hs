{-# language OverloadedStrings #-}
{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}


module Autolib.Logic.Formula.Doc where

import Autolib.ToDoc
import Data.String

-- should be in Autolib.ToDoc
instance IsString Doc where fromString = text

