{-# language DeriveDataTypeable #-}

module Autolib.LTL.Assign where

import Autolib.LTL.Type ( Name )

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Symbol
import Autolib.Size
import Autolib.Hash

import qualified Data.Map as M
import Data.Typeable

data Assign = Assign ( M.Map Name Bool )
    deriving ( Eq, Ord, Typeable )

lookup v (Assign m) = M.lookup v m

instance Symbol Assign where

instance Size Assign where size _ = 1
instance Hash Assign where 
    hash (Assign m) = hash m

instance Reader Assign where
    reader = assign

instance ToDoc Assign where
    toDoc (Assign m) = 
        braces $ fsep $ punctuate comma $ do
            (k,v) <- M.toList m
            return $ toDoc k <> text "=" <> toDoc v
instance Show Assign where show = render . toDoc

bind :: Parser (Name,Bool)
bind = do
    n <- reader
    my_symbol "="
    v <- reader
    return (n, v)

assign :: Parser Assign
assign = my_braces $ do
        bs <- my_commaSep bind
        return $ Assign $ M.fromList bs
