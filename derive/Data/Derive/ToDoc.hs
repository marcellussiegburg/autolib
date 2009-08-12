module Data.Derive.ToDoc (makeToDoc) where

{-

example :: Foo
instance ToDoc Foo where

-}
-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeToDoc :: Derivation
makeToDoc = derivationDSL "ToDoc" dslToDoc

dslToDoc =
    List [App "InstDecl" (List [List [],App "UnQual" (List [App
    "Ident" (List [String "ToDoc"])]),List [App "TyCon" (List [App
    "UnQual" (List [App "Ident" (List [String "Foo"])])])],List []])]
-- GENERATED STOP
