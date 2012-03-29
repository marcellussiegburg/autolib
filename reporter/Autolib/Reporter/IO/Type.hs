{-# language GeneralizedNewtypeDeriving, StandaloneDeriving, DeriveFunctor #-}

module Autolib.Reporter.IO.Type where

import qualified Autolib.Reporter.Classic.Type as Classic

import Control.Monad.Trans.Writer.Lazy
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class 
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

instance MonadIO Reporter where
    liftIO a = Reporter $ do x <- liftIO a ; return $ Just x

lift :: Classic.Reporter a -> Reporter a
lift r = Reporter $ do
    ( r, o ) <- liftIO $ Classic.run r
    tell o
    return r

output :: Output -> Reporter ()
output o = do
    Reporter $ do tell o ; return $ Just ()

inform :: Doc -> Reporter ()
inform = output . Doc

newline :: Reporter ()
newline = inform ( text " " )

reject :: Doc -> Reporter a
reject d = do 
    output $ Doc d
    Reporter $ return Nothing

nested :: Int -> Reporter a -> Reporter a
nested d ( Reporter r ) = 
    Reporter $ censor  Nest  r

-- | a reporter who always returns
wrap :: Reporter a -> Reporter ( Maybe a )
wrap ( Reporter r ) = Reporter $ do
    x <- r
    return $ Just x

-- | wenn ok, dann nichts sagen, sonst fehler melden
silent :: Reporter a -> Reporter a
silent ( Reporter r ) = Reporter $ pass $ do
    x <- r
    return $ case x of
        Nothing -> ( Nothing, id )
        Just a  -> ( Just a, const mempty )

run :: Reporter a -> IO ( Maybe a, Output )
run ( Reporter r ) = runWriterT r
        
result r = do
    (m, o ) <- run r
    return m
    
kommentar r = do
    (m, o ) <- run r
    return o

capture :: Reporter a 
        -> Reporter ( Maybe a, Output )
capture ( Reporter r ) = Reporter $ do
    (a, w) <- listen r
    return $ Just (a,w)
    
    


assert :: Bool -> Doc -> Reporter ()
assert p doc = do
    inform doc
    nested 4 $
         if p then inform $ multitext 
                          [ (DE, "Ja."), (UK, "Yes.") ]
              else reject $ multitext 
                          [ (DE, "Nein."), (UK, "No.") ]



execute a = Reporter $ do
   liftIO a
   return $ Just ()
