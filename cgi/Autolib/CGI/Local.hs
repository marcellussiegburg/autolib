-- | produce an HTML document with input forms.
-- for use in a session, all the state is in the CGI parameters.

module Web.Local

( Local, emit, local, global, collect 
, runLocalT
, W.open, W.close
)

where

import qualified Web.Store as W

import Control.Monad.State

type Local a = StateT ( W.Store a )

runLocalT :: Monad m => Local a m b -> m a
runLocalT f = do
    ( b, s ) <- runStateT ( do f ; collect ) W.empty
    return b

-- | add Html element to topmost container
emit :: Monad m 
     => a
     -> Local a m ()
emit h = do
    modify $ W.push h

-- | execute a statement in a fresh container,
-- after execution, apply combination function
-- and emit result to underlying container
local :: ( Monad m )
      => ( [a] -> a ) 
      -> Local a m b
      -> Local a m b
local combine action = do
    modify $ W.open combine
    x <- action
    modify $ W.close 
    return x

global :: Monad m
      => ( [a] -> a ) 
      -> Local a m b
      -> Local a m b
global combine action = do
    modify $ W.open combine
    action

collect :: Monad m => Local a m a
collect = do
    s <- get
    return $ W.collect s

