--
-- derive Reader instances
--

module Data.Derive.Reader (makeReader, deriveReader) where

import Data.DeriveTH
import Data.Derive.DSL.HSE
import Data.List
import qualified Language.Haskell as H

{-
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

custom = customSplice splice

splice :: FullDataDecl -> Exp -> Exp
splice d x | x ~= "reader" = let
    mkPzero = var "pzero"
    mkOr a b = InfixApp a (qvop "<|>") b
  in
    foldr mkOr mkPzero (map mkReadCon (dataDeclCtors (snd d)))

mkReadCon :: CtorDecl -> Exp
mkReadCon c = let
    fields = any (not . null . fst) (ctorDeclFields c)
    vars = map (("x"++) . show) [1..length (ctorDeclFields c)]
    body = concat (intersperse [comma] (zipWith field vars (ctorDeclFields c)))
        ++ [Qualifier $ H.App (var "return") $ apps (cName c) (map var vars)]
    field v (fn, _)
        | "_info" `isSuffixOf` fn = [
            Generator undefined (pVar v) (var "parsed_info")
        ]
        | fields = [
            Qualifier (var "my_reserved" `H.App` strE fn),
            Qualifier (var "my_equals"),
            Generator undefined (pVar v) (var "readerPrec" `H.App` mkInt 0)
        ]
        | otherwise = [
            Generator undefined (pVar v) (var "readerPrec" `H.App` mkInt 9)
        ]
    braces b = [Qualifier $ H.InfixApp (var "my_braces") (qvop "$") (Do b)]
    comma = Qualifier $ var "my_comma"
  in
    InfixApp (var "readerParenPrec" `H.App` var "p") (qvop "$") $
        Lambda undefined [pVar "p"] $ Do $
            [Qualifier $ var "guard" `H.App`
                 InfixApp (var "p") (qvop "<") (mkInt 9)] ++
            if fields then braces body else body

cName :: CtorDecl -> Exp
cName c = con (ctorDeclName c)

mkInt :: Integer -> Exp
mkInt = Lit . H.Int

deriveReader = derive makeReader
