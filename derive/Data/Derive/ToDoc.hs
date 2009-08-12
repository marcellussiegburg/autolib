--
-- derive ToDoc instances
--
-- based heavily on Data.Derive.Show
--
-- FEATURE: components ending "_info" are NOT shown
-- BUG: does not handle infix type constructors.
--
-- Author:  Bertram Felgenhauer
-- License: GPL 2.0
--

module Data.Derive.ToDoc (makeToDoc, deriveToDoc) where

import Data.DeriveTH
import Data.Maybe
import Data.List
import Control.Monad
import Data.Derive.DSL.HSE
import qualified Language.Haskell as H

{-
import Autolib.ToDoc.Class
import Autolib.ToDoc.Dutch

example :: Custom
instance ToDoc a => ToDoc (Sample a) where
    toDocPrec p (First)        = $(toDoc 0)
    toDocPrec p (Second x1 x2) = $(toDoc 1)
    toDocPrec p (Third x1)     = $(toDoc 2)
-}

-- GENERATED START

import Data.Derive.DSL.DSL
import Data.Derive.Internal.Derivation

makeToDoc :: Derivation
makeToDoc = derivationCustomDSL "ToDoc" custom $
    List [Instance ["ToDoc"] "ToDoc" (List [App "InsDecl" (List [App
    "FunBind" (List [MapCtor (App "Match" (List [App "Ident" (List [
    String "toDocPrec"]),List [App "PVar" (List [App "Ident" (List [
    String "p"])]),App "PParen" (List [App "PApp" (List [App "UnQual"
    (List [App "Ident" (List [CtorName])]),MapField (App "PVar" (List
    [App "Ident" (List [Concat (List [String "x",ShowInt FieldIndex])]
    )]))])])],App "Nothing" (List []),App "UnGuardedRhs" (List [App
    "SpliceExp" (List [App "ParenSplice" (List [App "App" (List [App
    "Var" (List [App "UnQual" (List [App "Ident" (List [String "toDoc"
    ])])]),App "Lit" (List [App "Int" (List [CtorIndex])])])])])]),App
    "BDecls" (List [List []])]))])])])]
-- GENERATED STOP

custom = customSplice splice

splice :: FullDataDecl -> Exp -> Exp
splice d (H.App x (H.Lit (H.Int y))) | x ~= "toDoc" = let
    fields = any (not . null . fst) (ctorDeclFields c)
    c = dataDeclCtors (snd d) !! fromInteger y
  in
    if fields then customRecord c else customData c

customData :: CtorDecl -> Exp
customData c | null (ctorDeclFields c) = cName c
             | otherwise = apps (var "docParen") [
    InfixApp (var "p") (qvop ">=") (mkInt 10),
    InfixApp (cName c) (qvop "</>") (
        var "fsep" `H.App` H.List (
            map (\i -> mkToDocPrec 10 (var ("x" ++ show i)))
                [1..length (ctorDeclFields c)]))]

customRecord :: CtorDecl -> Exp
customRecord c = apps (var "docParen") [
    InfixApp (var "p") (qvop ">=") (mkInt 10),
    InfixApp (cName c) (qvop "</>") (
        var "dutch_record" `H.App` H.List (
            catMaybes (zipWith singleField [1..] (ctorDeclFields c))))]
  where
    singleField nr (fn, _) = do
         guard $ not $ "_info" `isSuffixOf` fn
         return $ foldr1 (flip InfixApp (qvop "<+>")) $
             [mkText (mkString fn),
              var "equals",
              mkToDocPrec 0 (var ("x" ++ show nr))]

cName :: CtorDecl -> Exp
cName c = mkText (strE (ctorDeclName c))

mkInt :: Integer -> Exp
mkInt = Lit . H.Int

mkString :: String -> Exp
mkString = Lit . H.String

mkText :: Exp -> Exp
mkText = (var "text" `H.App`)

mkToDocPrec :: Integer -> Exp -> Exp
mkToDocPrec p a = var "toDocPrec" `H.App` mkInt p `H.App` a

deriveToDoc = derive makeToDoc
