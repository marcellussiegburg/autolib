-- |
-- Copyright:   (c) Bertram Felgenhauer 2009
-- License:     GPL 2.0
-- Stability:   experimental
-- Portability: portable
--
-- Derive 'Autolib.Reader.Class.Reader' instances. Example:
--
-- @
-- $(derive makeReader ''Type)
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
    derive,
    derives
) where

import Data.DeriveTH
import Data.Derive.DSL.HSE
import Data.List
import qualified Language.Haskell as H

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

custom = customSplice splice

splice :: FullDataDecl -> Exp -> Exp
splice d x | x ~= "reader" = let
    mkPzero = var "pzero"
    mkOr a b = InfixApp a (qvop "<|>") b
  in
    InfixApp (var "readerParenPrec" `H.App` var "p") (qvop "$") $
        Lambda noSrcLoc [pVar "p"] $
            foldr mkOr mkPzero (map mkReadCon (dataDeclCtors (snd d)))

-- parse a single constructor
mkReadCon :: CtorDecl -> Exp
mkReadCon c = let
    cn = ctorDeclName c
    fields = any (not . null . fst) (ctorDeclFields c)
    vars = map (("x"++) . show) [1..length (ctorDeclFields c)]
    parseCon = [Qualifier $ var "my_reserved" `H.App` strE cn]
    parseFields =
        concat (intersperse [comma] (zipWith field vars (ctorDeclFields c))) ++
        [Qualifier $ var "return" `H.App` apps (con cn) (map var vars)]
    field v (fn, _)
        | "_info" `isSuffixOf` fn = [
            Generator noSrcLoc (pVar v) (var "parsed_info")
        ]
        | fields = [
            Qualifier (var "my_reserved" `H.App` strE fn),
            Qualifier (var "my_equals"),
            Generator noSrcLoc (pVar v) (var "readerPrec" `H.App` intE 0)
        ]
        | otherwise = [
            Generator noSrcLoc (pVar v) (var "readerPrec" `H.App` intE 9)
        ]
    braces b = [Qualifier $ H.InfixApp (var "my_braces") (qvop "$") (Do b)]
    comma = Qualifier $ var "my_comma"
  in
    Do $
        [Qualifier $ var "guard" `H.App`
             InfixApp (var "p") (qvop "<") (intE 9)
        | not . null. ctorDeclFields $ c] ++
        parseCon ++
        (if fields then braces else id) parseFields

noSrcLoc :: SrcLoc
noSrcLoc = SrcLoc "<generated>" 0 0
