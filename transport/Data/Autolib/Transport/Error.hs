-- |
-- Custom Error monad.

module Data.Autolib.Transport.Error (
    Error (..)
) where

import Control.Applicative
import Control.Monad

-- | 'Error a' is isomorphic to @Either String a@ but could be extended
-- to encode more error conditions.
data Error a = Error String | Result a

instance Monad Error where
    Error e  >>= _ = Error e
    Result x >>= f = f x

    return = Result

    fail = Error

instance Functor Error where
    fmap = liftM

instance Applicative Error where
    pure  = return
    (<*>) = ap

