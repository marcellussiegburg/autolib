-- |
-- Copyright:   (c) Bertram Felgenhauer 2009
-- License:     GPL 2.0
-- Stability:   experimental
-- Portability: portable
--
-- Derive 'Autolib.Reader.Class.Reader' instances. Example:
--
-- @
-- $(derives [makeReader] [''Type])
-- @
--
-- Features:
--
--  * fields ending \"_info\" are handled by \"parse_info\"
--
-- Known bugs:
--
--  * infix type constructors are not handled correctly
--

module Data.Derive.Reader (
    -- * Derivations
    makeReader,
    -- * Reexports
    derives
) where

import Data.DeriveTH    (derives)
import Data.List        (intersperse, isSuffixOf)
import qualified Language.Haskell as H
import Language.Haskell (
    Exp, CtorDecl, Decl, FullDataDecl, Context,
    var, pVar, con, strE, intE, qvop, apps, (~=),
    ctorDeclName, ctorDeclFields, dataDeclCtors, dataDeclContext)

{-
import Autolib.Reader

example :: Custom
instance Reader a => Reader (Sample a) where
    atomic_readerPrec p = $(reader)
-}

-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeReader :: Derivation
makeReader = derivationCustomDSL "Reader" custom $
    List [Instance ["Reader"] "Reader" (List [App "InsDecl" (List [App
    "FunBind" (List [List [App "Match" (List [App "Ident" (List [
    String "atomic_readerPrec"]),List [App "PVar" (List [App "Ident" (
    List [String "p"])])],App "Nothing" (List []),App "UnGuardedRhs" (
    List [App "SpliceExp" (List [App "ParenSplice" (List [App "Var" (
    List [App "UnQual" (List [App "Ident" (List [String "reader"])])])
    ])])]),App "BDecls" (List [List []])])]])])])]
-- GENERATED STOP

-- ^ 'Derivation' for deriving 'Reader'

custom :: FullDataDecl -> [Decl] -> [Decl]
custom f = customSplice splice f . customContext context f

splice :: FullDataDecl -> Exp -> Exp
splice (_, d) x | x ~= "reader" = let
    mkPzero = var "pzero"
    mkOr a b = H.InfixApp a (qvop "<|>") b
  in
    H.InfixApp (var "readerParenPrec" `H.App` var "p") (qvop "$") $
        H.Lambda H.sl [pVar "p"] $
            foldr mkOr mkPzero (map mkReadCon (dataDeclCtors d))
splice _ e = error $ "makeReader: unrecognized splice: " ++ show e

-- parse a single constructor
mkReadCon :: CtorDecl -> Exp
mkReadCon c = let
    cn = ctorDeclName c
    fields = any (not . null . fst) (ctorDeclFields c)
    vars = map (("x"++) . show) [1..length (ctorDeclFields c)]
    parseCon = [H.Qualifier $ var "my_reserved" `H.App` strE cn]
    parseFields = parseFields' False vars (ctorDeclFields c)
    parseFields' c (v:vs) ((fn, _): fs)
        | "_info" `isSuffixOf` fn = [
            H.Generator H.sl (pVar v) (var "parsed_info")
        ] ++ parseFields' c vs fs
        | fields = [
            H.Qualifier (var "my_comma")
            | c
        ] ++ [
            H.Qualifier (var "my_reserved" `H.App` strE fn),
            H.Qualifier (var "my_equals"),
            H.Generator H.sl (pVar v) (var "readerPrec" `H.App` intE 0)
        ] ++ parseFields' True vs fs
        | otherwise = [
            H.Generator H.sl (pVar v) (var "readerPrec" `H.App` intE 9)
        ] ++ parseFields' c vs fs
    parseFields' _ _ _ =
        [H.Qualifier $ var "return" `H.App` apps (con cn) (map var vars)]
    braces b = [H.Qualifier $ H.InfixApp (var "my_braces") (qvop "$") (H.Do b)]
  in
    H.Do $
        [H.Qualifier $ var "guard" `H.App`
             H.InfixApp (var "p") (qvop "<") (intE 9)
        | not . null. ctorDeclFields $ c] ++
        parseCon ++
        (if fields then braces else id) parseFields

context :: FullDataDecl -> Context -> Context
context (_, d) ctx = dataDeclContext d ++ ctx
