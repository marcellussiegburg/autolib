{-# language NoMonomorphismRestriction, ScopedTypeVariables #-}

-- | produce an HTML document with input forms.
-- for use in a session, all the state is in the CGI parameters.

module Web.Form 

( Form
, render, local, emit 
, Web.Form.text , glue
, gensym
)

where

import Web.Local ( Local )
import qualified Web.Local as L

import Text.XHtml

import Control.Monad.State
import Control.Monad.Error


type Env = [(String,String)]

type Form m = StateT Int
                 ( ErrorT String 
                              ( Local Html m))

-- text :: Monad m => String -> Form m ()
text cs = emit $ Text.XHtml.stringToHtml cs

glue :: [Html] -> Html
glue = foldr1 (+++)

local = L.local
emit = lift . lift . L.emit

gensym :: Monad m 
       => String 
       -> Form m String
gensym prefix = do
    i <- get
    put $ succ i
    return $ prefix ++ show i

render :: Monad m 
       => Form m ()
       -> m Html
render ( f0 :: Form m () ) = do
    let f1 :: ErrorT String (Local Html m) ((), Int)
        f1 = runStateT f0 0

    let f2 :: Local Html m ( Either String ((), Int))
        f2 = runErrorT f1

    let f3 :: m Html
        f3 = L.runLocalT $ L.global glue $ f2

    f3








