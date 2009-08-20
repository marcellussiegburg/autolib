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

import Data.DeriveTH    (derive, derives)
import Data.List        (intersperse, isSuffixOf)
import qualified Language.Haskell as H
import Language.Haskell (
    SrcLoc, Exp, CtorDecl, Decl, FullDataDecl, Context,
    var, pVar, con, strE, intE, qvop, apps, (~=),
    ctorDeclName, ctorDeclFields, dataDeclCtors)
import qualified Data.Derive.Util.Missing as M

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
custom f = customSplice splice f . M.customContext' context f

splice :: FullDataDecl -> Exp -> Exp
splice (_, d) x | x ~= "reader" = let
    mkPzero = var "pzero"
    mkOr a b = H.InfixApp a (qvop "<|>") b
  in
    H.InfixApp (var "readerParenPrec" `H.App` var "p") (qvop "$") $
        H.Lambda noSrcLoc [pVar "p"] $
            foldr mkOr mkPzero (map mkReadCon (dataDeclCtors d))
splice _ e = error $ "makeReader: unrecognized splice: " ++ show e

-- parse a single constructor
mkReadCon :: CtorDecl -> Exp
mkReadCon c = let
    cn = ctorDeclName c
    fields = any (not . null . fst) (ctorDeclFields c)
    vars = map (("x"++) . show) [1..length (ctorDeclFields c)]
    parseCon = [H.Qualifier $ var "my_reserved" `H.App` strE cn]
    parseFields =
        concat (intersperse [comma] (zipWith field vars (ctorDeclFields c))) ++
        [H.Qualifier $ var "return" `H.App` apps (con cn) (map var vars)]
    field v (fn, _)
        | "_info" `isSuffixOf` fn = [
            H.Generator noSrcLoc (pVar v) (var "parsed_info")
        ]
        | fields = [
            H.Qualifier (var "my_reserved" `H.App` strE fn),
            H.Qualifier (var "my_equals"),
            H.Generator noSrcLoc (pVar v) (var "readerPrec" `H.App` intE 0)
        ]
        | otherwise = [
            H.Generator noSrcLoc (pVar v) (var "readerPrec" `H.App` intE 9)
        ]
    braces b = [H.Qualifier $ H.InfixApp (var "my_braces") (qvop "$") (H.Do b)]
    comma = H.Qualifier $ var "my_comma"
  in
    H.Do $
        [H.Qualifier $ var "guard" `H.App`
             H.InfixApp (var "p") (qvop "<") (intE 9)
        | not . null. ctorDeclFields $ c] ++
        parseCon ++
        (if fields then braces else id) parseFields

context :: FullDataDecl -> Context -> Context
context (_, d) ctx = M.dataDeclContext d ++ ctx

noSrcLoc :: SrcLoc
noSrcLoc = H.SrcLoc "<generated>" 0 0
