{-# language GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveFunctor #-}

module Autolib.Reporter.IO.Type where

import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.Maybe
import Control.Monad.Trans (lift )
import Data.Monoid

import Autolib.Output
import Autolib.Multilingual
import Autolib.ToDoc hiding ( render )

newtype Reporter a = 
        Reporter ( WriterT Output IO  ( Maybe a ) )

deriving instance Functor Reporter

instance Monad Reporter where
    return x = Reporter $ return $ Just x
    Reporter r >>= f = Reporter $ do
        m <- r
        case m of
            Nothing -> return Nothing
            Just x   -> let Reporter s = f x in s


run ( Reporter r ) = do
    ( a, o ) <- runWriterT r
    return ( a, render o )

output :: Output -> Reporter ()
output o = do
    Reporter $ do tell o ; return $ Just ()

inform :: Doc -> Reporter ()
inform = output . Doc

reject :: Doc -> Reporter ()
reject d = do 
    output $ Doc d
    Reporter $ return Nothing
    return () 

nested :: Int -> Reporter a -> Reporter a
nested d ( Reporter r ) = 
    Reporter $ mapMaybeT ( censor  Nest ) r

-- | a reporter who always returns
wrap :: Reporter a -> Reporter ( Maybe a )
wrap ( Reporter r ) = Reporter $ 
    mapMaybeT undefined r -- FIXME

-- | wenn ok, dann nichts sagen, sonst fehler melden
silent :: Reporter a -> Reporter a
silent r = r -- FIXME


export = error "Autolib.Reporter.IO.Type has no pure export"
kommentar = error "Autolib.Reporter.IO.Type has no pure kommentar"


action = error "Autolib.Reporter.IO.Type.action must be run"

execute a = Reporter $ lift a

