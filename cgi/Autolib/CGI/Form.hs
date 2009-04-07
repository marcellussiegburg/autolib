{-# language NoMonomorphismRestriction, ScopedTypeVariables, PatternSignatures #-}

-- | produce an HTML document with input forms.
-- for use in a session, all the state is in the CGI parameters.

module Autolib.CGI.Form 

( Form
, render, local, emit, glue
, gensym, gensym_pref
)

where

import Autolib.CGI.Local ( Local )
import qualified Autolib.CGI.Local as L

import Text.XHtml (Html)
import qualified Text.XHtml as X

import Control.Monad.State
import Control.Monad.Error

-- | This is the monad (it should really be abstract).
--
-- 'StateT' is needed to count up the widget numbers
-- (they start with zero for each page, and it is important that
-- on each page generation, numbers are identical: 
-- the session state is stored in the input and hidden widgets,
-- and it needs to be found again)
--
-- 'ErrorT' is needed because we want to stop page generation
-- on error (e.g. if some required input is missing).
-- 
-- 'Local' is needed to properly close all HTML elements
--
-- the parameter 'm' is a monad (it would typically be the state monad from happstack)

type Form m = StateT Int
                 ( ErrorT String 
                       ( Local Html m ))

local combine action = do
    lift $ lift $ modify $ L.open combine
    x <- action
    lift $ lift $ modify $ L.close 
    return x

emit = lift . lift . L.emit

gensym = gensym_pref "G"

gensym_pref :: Monad m 
       => String 
       -> Form m String
gensym_pref prefix = do
    i <- get
    put $ succ i
    return $ prefix ++ show i


glue :: [Html] -> Html
glue = X.concatHtml


render :: Monad m 
       => Form m ()
       -> m Html
render ( f0 :: Form m () ) = do
    let f1 :: ErrorT String (Local Html m) ((), Int)
        f1 = runStateT f0 0

    let f2 :: Local Html m ( Either String ((), Int))
        f2 = runErrorT f1

    let f3 :: m (Html, Either String ((), Int))
        f3 = L.runLocalT $ L.global glue $ f2

    ( h, x ) <- f3
    return $ X.form h X.! [ X.method "POST" , X.enctype "multipart/form-data" ]









