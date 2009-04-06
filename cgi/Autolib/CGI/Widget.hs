{-# language NoMonomorphismRestriction, ScopedTypeVariables #-}

module Web.Widget 

( Form, render, getenv
, text 
, table, btable, td, tr, row
, textfield, password, submit
, textarea
, menu
, h3, h2, h1, br, hr
-- convenience re-exports
, ServerMonad, MonadPlus
)

where

import Prelude hiding ( concat )

import Web.Form

import Happstack.Server hiding ( look )

import qualified Text.XHtml as X
import Control.Monad.Reader hiding ( local )
import Data.ByteString.Lazy.Char8 ( unpack )
import Data.Maybe ( isJust, catMaybes, listToMaybe )
import Control.Monad ( guard, mzero )
import Data.List ( isPrefixOf )

newtype Env = Env [(String,String)]

instance FromData Env where
    fromData = do 
        ( env, cook ) <- ask ; 
        return $ Env $ until_submit $ do (s,i) <- env ; return (s, unpack $ inputValue i)

until_submit pairs = 
    let ( pre , post ) = span ( \ (tag,val) -> not $ "S" `isPrefixOf` tag ) pairs
    in  pre ++ take 1 post

getenv = withData $ \ (Env env) -> do
    return env 

look tag = withData $ \ (Env env) -> do
    return $ lookup tag env

----------------------------------------------------------------------------------

-- * nonblocking input widgets

-- | for one-line input text. with default. input is echoed.
-- this widget is non-blocking (always successful)
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

-- | like textfield, but don't show the entry.
password :: ( ServerMonad m, MonadPlus m ) 
          => String -> Form m String
password cs = do
    tag :: String <- gensym
    val :: Maybe String <- lift $ lift $ lift $ look tag
    let ds :: String
        ds = case val of
               Nothing -> cs
               Just cs -> cs
    emit $ X.password tag X.! [ X.value ds ]
    return ds

-- | multi-line text input, with default.
-- the number of rows is slightly larger than the number of lines of the default,
-- the number of columns is slightly larger than the maximun line width of the defaul.
textarea :: ( ServerMonad m, MonadPlus m ) 
          => String -> Form m String
textarea cs = do
    tag :: String <- gensym
    val :: Maybe String <- lift $ lift $ lift $ look tag
    let ds :: String
        ds = case val of
               Nothing -> cs
               Just cs -> cs
    let dss = lines ds
        cols = length dss
        rows = maximum $ 0 : map length dss
    emit $ X.textarea ( X.stringToHtml ds )
           X.! [ X.name tag 
               , X.cols $ show (cols + 10)
               , X.rows $ show (rows + 2) 
               ]
    return ds

-- | a submit button, with label.
-- This widget is non-blocking.
-- A blocking effect can be achieved with "do c <- submit "check"; guard c"
submit :: ( ServerMonad m, MonadPlus m ) 
       => String
       -> Form m Bool
submit cs = do
    tag :: String <- gensym_pref "S"
    val :: Maybe String <- lift $ lift $ lift $ look tag
    emit $ X.submit tag cs
    return $ isJust val


-- * blocking input widgets

-- | if this menu is met for the first time, it blocks
-- (a selection must be made).
-- The previous selection is remembered (in a hidden element)
-- and subsequent visits are non-blocking, taking the previous selection as default.
-- This menu is a table row consisting of three table data elements:
-- the name of the menu, the options, and the chosen option.
-- The menu should be surrounded by table environment.
menu ::  ( ServerMonad m, MonadPlus m ) 
       => String 
       -> [ (String, a) ]
       -> Form m a
menu title options = tr $ do
    tag <- gensym_pref "H"
    mprev <- lift $ lift $ lift $ look tag
    td $ text title
    items <- td $ forM options $ \ ( name, value ) -> do
        s <- submit name
        return $ do guard ( s || Just name == mprev ) ; return ( name, value )
    case catMaybes items of
         [ ( name, value ) ] -> td $ do
             text name
             emit $ X.hidden tag name
             return $ value
         _ -> mzero


-- * Formatting atoms

-- | line break
br ::  ( ServerMonad m, MonadPlus m ) 
          => Form m ()
br = emit $ X.br

-- | horizontal ruler
hr ::  ( ServerMonad m, MonadPlus m ) 
          => Form m ()
hr = emit $ X.hr

-- | heading level 3 (weak)
h3 ::  ( ServerMonad m, MonadPlus m ) 
          => String -> Form m ()
h3 cs = emit $ X.h3 $ X.stringToHtml cs

-- | heading level 2 (medium)
h2 ::  ( ServerMonad m, MonadPlus m ) 
          => String -> Form m ()
h2 cs = emit $ X.h2 $ X.stringToHtml cs

-- | heading level 1 (strong)
h1 ::  ( ServerMonad m, MonadPlus m ) 
          => String -> Form m ()
h1 cs = emit $ X.h1 $ X.stringToHtml cs

-- | text outout
text :: ( ServerMonad m, MonadPlus m ) 
          => String -> Form m ()
text cs = emit $ X.stringToHtml cs


-- * formatting combinators
-- these will be applied to monadic actions.
-- each monadic action (possibly) produces an output list

-- | all the output in this group is appended, resulting in one item.
concat :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
concat   = local ( glue  )

-- | concat all group items and wrap in "table data" element
td :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
td   = local ( X.td . glue )

-- | concat all group items and wrap in "table row" element
tr :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
tr = local ( X.tr . glue ) 

-- | for backward compatibility: each item in "table data", concatenation in "table row"
row :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
row = local ( X.tr . glue . map X.td ) 


-- | concat all group items and wrap in "table" element
table :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
table = local ( X.table . glue ) 

-- | concat all group items and wrap in "table" element with border
btable :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
btable = local ( ( X.! [ X.border 1 ] ) . X.table . glue ) 
