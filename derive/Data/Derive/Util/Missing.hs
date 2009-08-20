-- |
-- Copyright:   (c) Bertram Felgenhauer 2009
-- License:     GPL 2.0
-- Stability:   experimental
-- Portability: portable
--
-- Some utility functions that should be part of Data.Derive.
--
module Data.Derive.Util.Missing where

import Language.Haskell (
    Context, FullDataDecl, DataDecl, Decl (InstDecl, DataDecl))

-- Should be in Language.Haskell.
dataDeclContext :: DataDecl -> Context
dataDeclContext (DataDecl _ _ ctx _ _ _ _) = ctx
dataDeclContext _ = error "dataDeclContext: not a DataDecl"

-- Should be in Data.Derive.Internal.Derivation.
-- The difference to customContext is that the unmodified context
-- is passed to the customizing function for reuse.
customContext' :: (FullDataDecl -> Context -> Context)
               -> (FullDataDecl -> [Decl] -> [Decl])
customContext' custom d = map f
    where
        f (InstDecl sl ctx a b c) = InstDecl sl (custom d ctx) a b c
        f x = x
