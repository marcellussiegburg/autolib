{-# language NoMonomorphismRestriction, ScopedTypeVariables, PatternSignatures #-}


module Autolib.CGI.Widget 

( Form, render, getenv
-- * nonblocking input widgets
, textfield, empty_textfield
, password, submit
, textarea, empty_textarea
-- * blocking input widgets
, menu, nonblocking_menu
-- * formatting combinators
, table, btable, td, tr, row, p
-- * Formatting atoms
, text 
, h3, h2, h1, br, hr
-- * directly output XHtml stuff
, xhtml
-- * convenience re-exports
, ServerMonad, MonadPlus
)



where

import Prelude hiding ( concat )

import Autolib.CGI.Form

import Happstack.Server hiding ( look )

import qualified Text.XHtml as X
import Control.Monad.Reader hiding ( local )
import Data.ByteString.Lazy.Char8 ( unpack )
import Data.Maybe ( isJust, catMaybes, listToMaybe, fromMaybe )
import Control.Monad ( guard, mzero )
import Data.List ( isPrefixOf )

import qualified Codec.Binary.UTF8.String as CBUS

import qualified Data.ByteString.Lazy.Char8      as L
import qualified Data.ByteString.Lazy.UTF8       as LU (toString, fromString)


newtype Env = Env [(String,String)]

instance FromData Env where
    fromData = do 
        ( env, bodies, cookies ) <- askRqEnv ; 
        return $ Env $ until_submit $ do 
             (s , i @ Input { inputValue = Right bs }) <- 
                  env ++ fromMaybe [] bodies
             return (s, unpack bs)

until_submit pairs = 
    let ( pre , post ) = span ( \ (tag,val) -> not $ "S" `isPrefixOf` tag ) pairs
    in  pre ++ take 1 post

getenv = withData $ \ (Env env) -> do
    return env 

look tag = withData $ \ (Env env) -> do
    return $ lookup tag env

----------------------------------------------------------------------------------

-- | for one-line input text. with default. input is echoed.
-- this widget is non-blocking (always successful)
textfield :: ( MonadPlus m, MonadIO m, HasRqData m, ServerMonad m ) 
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


empty_textfield :: ( MonadPlus m, MonadIO m, HasRqData m, ServerMonad m ) 
          => Int -- ^ size
          -> Form m String
empty_textfield s = do
    tag :: String <- gensym
    val :: Maybe String <- lift $ lift $ lift $ look tag
    let ds :: String
        ds = case val of
               Nothing -> ""
               Just cs -> cs
    emit $ X.textfield tag X.! [ X.value ds, X.size $ show s ]
    return ds

-- | like textfield, but don't show the entry.
password :: ( MonadPlus m, MonadIO m, HasRqData m, ServerMonad m ) 
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
-- the number of columns is slightly larger than the maximun line width of the default.
textarea :: ( MonadPlus m, MonadIO m, HasRqData m, ServerMonad m ) 
          => String -> Form m String
textarea  cs = do
    tag :: String <- gensym
    val <- lift $ lift $ lift $ look tag
    let ds = case val of
               Nothing -> cs
               Just cs -> cs
    let dss = lines ds
        rows = length dss
        cols = maximum $ 0 : map length dss
    emit $ X.textarea ( X.primHtml $ CBUS.decodeString ds )
           X.! [ X.name tag 
               , X.cols $ show (cols + 10)
               , X.rows $ show (rows + 2) 
               ]
    return ds

empty_textarea ::  ( MonadPlus m, MonadIO m, HasRqData m, ServerMonad m ) 
          => Int -- ^ rows
          -> Int -- ^ cols
          -> Form m String
empty_textarea rows cols = do
    tag :: String <- gensym
    val :: Maybe String <- lift $ lift $ lift $ look tag
    let ds :: String
        ds = case val of
               Nothing -> ""
               Just cs -> cs
    emit $ X.textarea ( X.primHtml $ CBUS.decodeString ds )
           X.! [ X.name tag 
               , X.cols $ show cols
               , X.rows $ show rows
               ]
    return ds

-- | a submit button, with label.
-- This widget is non-blocking.
-- A blocking effect can be achieved with 
-- 
-- > do c <- submit "check"; guard c
-- 
submit ::  ( MonadPlus m, MonadIO m, HasRqData m, ServerMonad m ) 
       => String
       -> Form m Bool
submit cs = submit_pref "S" cs

submit_pref pref cs = do
    tag :: String <- gensym_pref pref
    val :: Maybe String <- lift $ lift $ lift $ look tag
    emit $ X.submit tag cs
    return $ isJust val


-- | if this menu is met for the first time, it blocks
-- (a selection must be made).
-- The previous selection is remembered (in a hidden element)
-- and subsequent visits are non-blocking, taking the previous selection as default.
-- This menu is a table row consisting of three table data elements:
-- the name of the menu, the options, and the chosen option.
-- The menu should be surrounded by table environment.

menu ::  ( MonadPlus m, MonadIO m, HasRqData m, ServerMonad m ) 
       => String 
       -> [ (String, a) ]
       -> Form m a
menu title options = tr $ do
    tag <- gensym_pref "H"
    mprev <- lift $ lift $ lift $ look tag
    td $ text title
    items <- td $ forM ( zip [0..] options ) $ \ ( i, ( name, value ) ) -> do
        s <- submit name
        return $ do guard ( s || Just ( show i ) == mprev ) ; return ( i, name, value )
    case catMaybes items of
         [ ( i, name, value ) ] -> td $ do
             text name
             emit $ X.hidden tag ( show i )
             return $ value
         _ -> mzero

-- | same as above, but nonblocking. 
-- the first option is the default.
-- be very careful: the numbering of the widges in the rest of the page
-- must not depend on the output of this (or any other nonblocking) widget!
-- TODO: use the type system to enforce this.
nonblocking_menu ::  ( MonadPlus m, MonadIO m, HasRqData m, ServerMonad m ) 
       => String 
       -> [ (String, a) ]
       -> Form m a
nonblocking_menu title options = tr $ do
    tag <- gensym_pref "H"
    mprev <- lift $ lift $ lift $ look tag
    td $ text title
    items <- td $ forM ( zip [0..] options ) $ \ ( i, ( name, value ) ) -> do
        s <- submit_pref "N" name
        return ( i, name, value, s )
    let clicked = do ( i, n, v, True ) <- items ; return ( i, n, v )
        remembered = do
            ( i, n, v, s ) <- items
            guard $ Just ( show i ) == mprev
            return ( i, n, v )
    let ( i, name, value ) = case ( clicked, remembered ) of
         ( cl : icks , _ ) -> cl
         ( [], r : ems ) -> r
         ( [], [] ) -> ( \ ( n, v ) -> ( 0, n, v ) ) $ head options
    td $ do
             text name
             emit $ X.hidden tag ( show i )
             return $ value

-- | direct output of XHtml stuff
xhtml :: ( ServerMonad m, MonadPlus m ) 
          => X.Html -> Form m ()
xhtml = emit

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


-- | all the output in this group is appended, resulting in one item.
concat :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
concat   = local ( glue  )

-- | concat all group items and wrap in table data element
td :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
td   = local ( X.td . glue )

-- | concat all group items and wrap in table row element
tr :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
tr = local ( X.tr . glue ) 


-- | concat all group items and wrap in table paragraph element
p :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
p = local ( X.p . glue ) 

-- | for backward compatibility: each item in table data, concatenation in table row
row :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
row = local ( X.tr . glue . map X.td ) 


-- | concat all group items and wrap in table element
table :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
table = local ( X.table . glue ) 

-- | concat all group items and wrap in table element with border
btable :: ( ServerMonad m, MonadPlus m ) 
          => Form m a -> Form m a
btable = local ( ( X.! [ X.border 1 ] ) . X.table . glue ) 
