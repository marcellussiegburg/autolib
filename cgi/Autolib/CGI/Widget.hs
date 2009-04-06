{-# language NoMonomorphismRestriction, ScopedTypeVariables #-}

module Web.Widget 

( Form, render
, text 
, table, btable, row
, textfield, submit
, h3, h2, h1, br, hr
)

where

import Web.Form

import Happstack.Server hiding ( look )

import qualified Text.XHtml as X
import Control.Monad.Reader hiding ( local )
import Data.ByteString.Lazy.Char8 ( unpack )


newtype Env = Env [(String,String)]

instance FromData Env where
    fromData = do 
        ( env, cook ) <- ask ; 
        return $ Env $ do (s,i) <- env ; return (s, unpack $ inputValue i)

look tag = withData $ \ (Env env) -> do
    return $ lookup tag env

textfield :: ( ServerMonad m, MonadPlus m ) 
          => String -> Form m String
textfield cs = do
    tag :: String <- gensym
    val :: Maybe String <- lift $ lift $ lift $ look tag
    let ds :: String
        ds = case val of
               Nothing -> cs
               Just cs -> cs
    emit $ X.textfield tag X.! [ X.value ds ]
    return ds

submit :: ( ServerMonad m, MonadPlus m ) 
       => String
       -> Form m ( Maybe String )
submit cs = do
    tag :: String <- gensym
    val :: Maybe String <- lift $ lift $ lift $ look tag
    emit $ X.submit tag cs
    return val

br ::  ( ServerMonad m, MonadPlus m ) 
          => Form m ()
br = emit $ X.br

hr ::  ( ServerMonad m, MonadPlus m ) 
          => Form m ()
hr = emit $ X.hr

h3 ::  ( ServerMonad m, MonadPlus m ) 
          => String -> Form m ()
h3 cs = emit $ X.h3 $ X.stringToHtml cs

h2 ::  ( ServerMonad m, MonadPlus m ) 
          => String -> Form m ()
h2 cs = emit $ X.h2 $ X.stringToHtml cs

h1 ::  ( ServerMonad m, MonadPlus m ) 
          => String -> Form m ()
h1 cs = emit $ X.h1 $ X.stringToHtml cs

text :: ( ServerMonad m, MonadPlus m ) 
          => String -> Form m ()
text cs = emit $ X.stringToHtml cs

row :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
row   = local ( glue . map X.td )

table :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
table = local ( X.table . glue . map X.tr ) 

-- | table with border
btable :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
btable = local ( ( X.! [ X.border 1 ] ) . X.table . glue . map X.tr ) 
