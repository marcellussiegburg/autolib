{-# language TypeSynonymInstances #-}

module Autolib.CGI.Edit where

import Autolib.CGI.Widget
import Control.Monad (mzero)

class Edit a where 
    edit :: ( ServerMonad m, MonadPlus m ) 
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


        
