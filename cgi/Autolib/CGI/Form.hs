{-# language NoMonomorphismRestriction, ScopedTypeVariables #-}

-- | produce an HTML document with input forms.
-- for use in a session, all the state is in the CGI parameters.

module Web.Form 

( render, local, emit 
, Web.Form.text , glue
)

where

import Web.Local ( Local )
import qualified Web.Local as L

import Text.XHtml

import Control.Monad.State


type Env = [(String,String)]

type Form m = StateT Int
                    ( Local Html m )

-- text :: Monad m => String -> Form m ()
text cs = emit $ Text.XHtml.stringToHtml cs

glue :: [Html] -> Html
glue = foldr1 (+++)

local = L.local
emit = lift . L.emit

render :: Monad m 
       => Form m ()
       -> m Html
render ( f0 :: Form m () ) = do
    let f1 :: Local Html m ((), Int)
        f1 = runStateT f0 0
    let f4 :: m Html
        f4 = L.runLocalT $ L.global glue f1
    x <- f4
    return x






