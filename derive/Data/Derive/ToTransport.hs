-- |
-- Copyright:   (c) Bertram Felgenhauer 2009
-- License:     GPL 2.0
-- Stability:   experimental
-- Portability: portable
--
-- Derive 'Data.Autolib.Transport.Class.ToTransport' instances. Example:
--
-- @
-- $(derives [makeToTransport] [''Type])
-- @
--

module Data.Derive.ToTransport (
    -- * Derivations
    makeToTransport,
    -- * Reexports
    derives
) where

import Data.DeriveTH (derives)
import qualified Language.Haskell as H
import Language.Haskell (
    Exp, Pat, Alt, CtorDecl, Decl, FullDataDecl, FieldDecl, BangType, Stmt,
    (~=), var, pVar, con, strE, strP, apps, qname,
    ctorDeclFields, ctorDeclName, dataDeclCtors)

{-
import Data.Autolib.Transport

example :: Custom
instance ToTransport a => ToTransport (Sample a) where
    fromTransport (TrObject x) = $(fromTransport)
    fromTransport _            = $(failFromTransport)
    toTransport (First)        = $(toTransport 0)
    toTransport (Second x1 x2) = $(toTransport 1)
    toTransport (Third x1)     = $(toTransport 2)
-}

-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeToTransport :: Derivation
makeToTransport = derivationCustomDSL "ToTransport" custom $
    List [Instance ["ToTransport"] "ToTransport" (List [App "InsDecl"
    (List [App "FunBind" (List [List [App "Match" (List [App "Ident" (
    List [String "fromTransport"]),List [App "PParen" (List [App
    "PApp" (List [App "UnQual" (List [App "Ident" (List [String
    "TrObject"])]),List [App "PVar" (List [App "Ident" (List [String
    "x"])])]])])],App "Nothing" (List []),App "UnGuardedRhs" (List [
    App "SpliceExp" (List [App "ParenSplice" (List [App "Var" (List [
    App "UnQual" (List [App "Ident" (List [String "fromTransport"])])]
    )])])]),App "BDecls" (List [List []])]),App "Match" (List [App
    "Ident" (List [String "fromTransport"]),List [App "PWildCard" (
    List [])],App "Nothing" (List []),App "UnGuardedRhs" (List [App
    "SpliceExp" (List [App "ParenSplice" (List [App "Var" (List [App
    "UnQual" (List [App "Ident" (List [String "failFromTransport"])])]
    )])])]),App "BDecls" (List [List []])])]])]),App "InsDecl" (List [
    App "FunBind" (List [MapCtor (App "Match" (List [App "Ident" (List
    [String "toTransport"]),List [App "PParen" (List [App "PApp" (List
    [App "UnQual" (List [App "Ident" (List [CtorName])]),MapField (App
    "PVar" (List [App "Ident" (List [Concat (List [String "x",ShowInt
    FieldIndex])])]))])])],App "Nothing" (List []),App "UnGuardedRhs"
    (List [App "SpliceExp" (List [App "ParenSplice" (List [App "App" (
    List [App "Var" (List [App "UnQual" (List [App "Ident" (List [
    String "toTransport"])])]),App "Lit" (List [App "Int" (List [
    CtorIndex])])])])])]),App "BDecls" (List [List []])]))])])])]
-- GENERATED STOP

-- ^ 'Derivation' for 'ToTransport'

custom :: FullDataDecl -> [Decl] -> [Decl]
custom = customSplice splice

splice :: FullDataDecl -> Exp -> Exp
splice d x | x ~= "fromTransport" = mkFrom d
splice d x | x~= "failFromTransport" = mkFail d
splice d (H.App x (H.Lit (H.Int y))) | x~= "toTransport" = mkTo d y
splice _ e = error $ "makeToTransport: unrecognized splice: " ++ show e

------------------------------------------------------------------------------
-- toTransport

mkTo :: FullDataDecl -> Integer -> Exp
mkTo d y = let
    hasFields = any (not . null . fst) (ctorDeclFields c)
    c = dataDeclCtors (snd d) !! fromInteger y
    mkFields = if hasFields then mkShowRecordFields else mkShowPlainFields
  in
    mkObject $ H.List
        [H.Tuple [strE (ctorDeclName c), mkFields (ctorDeclFields c)]]

mkFail :: FullDataDecl -> Exp
mkFail d = var "fail" `H.App` strE ("malformed encoding for '"
    ++ H.dataDeclName (snd d) ++ "' (expected Object)")

mkShowPlainFields :: FieldDecl -> Exp
mkShowPlainFields fs = mkArray $ H.List
    [var "toTransport" `H.App` xi | xi <- vars "x" fs]

mkShowRecordFields :: FieldDecl -> Exp
mkShowRecordFields fs = mkObject $ H.List
    [ H.Tuple [strE fn, var "toTransport" `H.App` xi]
    | ((fn, _), xi) <- zip fs (vars "x" fs)]

------------------------------------------------------------------------------
-- fromTransport


mkFrom :: FullDataDecl -> Exp
mkFrom (_, d) = let
    fromError = var "fail" `H.App` strE "malformed Trans for type ...."
  in
    H.Case (var "x") $
    map mkFromCtor (dataDeclCtors d) ++
    [H.Alt H.sl H.PWildCard (H.UnGuardedAlt fromError) (H.BDecls [])]

mkFromCtor :: CtorDecl -> Alt
mkFromCtor c = let
    cn = ctorDeclName c
    fs = ctorDeclFields c
    hasFields = any (not . null . fst) fs
    body | hasFields = mkFromRecord cn fs
         | otherwise = mkFromPlain cn fs
  in
    H.Alt H.sl (H.PList [H.PTuple [strP cn, pVar "y"]])
         (H.UnGuardedAlt body) (H.BDecls [])

mkFromRecord :: String -> FieldDecl -> Exp
mkFromRecord cn fs = H.Do $
    [H.Generator H.sl (H.PApp (qname "TrObject") [pVar "z"])
          (var "return" `H.App` var "y")] ++
    [H.LetStmt $ H.BDecls [H.PatBind H.sl (pVar "d") Nothing
          (H.UnGuardedRhs $ var "z")
          (H.BDecls [])]] ++
    zipWith (mkFromRecordField cn) (pVars "x" fs) fs ++
    mkFromTrailer cn fs

mkFromRecordField :: String -> Pat -> (String, BangType) -> Stmt
mkFromRecordField cn xi (fn, _) = H.Generator H.sl xi $
    apps (var "maybe") [
        var "fail" `H.App` strE (unwords ["fromTransport: missing field", fn,
                                          "while decoding a", cn]),
        var "return",
        apps (var "lookup") [strE fn, var "d"]]

mkFromPlain :: String -> FieldDecl -> Exp
mkFromPlain cn fs = H.Do $
    [H.Generator H.sl (H.PApp (qname "TrArray") [H.PList (pVars "x" fs)])
        (var "return" `H.App` var "y")] ++
    mkFromTrailer cn fs

mkFromTrailer :: String -> FieldDecl -> [Stmt]
mkFromTrailer cn fs =
    [ H.Generator H.sl yi (var "fromTransport" `H.App` xi)
    | (xi, yi) <- zip (vars "x" fs) (pVars "y" fs)] ++
    [H.Qualifier $ var "return" `H.App` apps (con cn) (vars "y" fs)]

------------------------------------------------------------------------------
-- utilites

mkObject :: Exp -> Exp
mkObject e = con "TrObject" `H.App` e

mkArray :: Exp -> Exp
mkArray e = con "TrArray" `H.App` e

vars :: String -> FieldDecl -> [Exp]
vars pre fs = [var (pre ++ show i) | i <- [1..length fs]]

pVars :: String -> FieldDecl -> [Pat]
pVars pre fs = [pVar (pre ++ show i) | i <- [1..length fs]]
