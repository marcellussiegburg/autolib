{-# language TypeSynonymInstances #-}

module Autolib.CGI.Edit where

import Autolib.CGI.Widget
import Happstack.Server ( HasRqData, ServerMonad )
import Control.Monad (mzero )
import Control.Monad.IO.Class

class Edit a where 
    edit ::  ( MonadPlus m, MonadIO m, HasRqData m, ServerMonad m ) 
         => a
         -> Form m a
    
instance Edit String where
    edit cs = textfield cs

instance Edit Integer where
    edit n = do
        ds <- textfield $ show n
        case reads ds of
            [(n, "")] -> return n
            _ -> mzero


        
