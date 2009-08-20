-- |
-- Copyright:   (c) Bertram Felgenhauer 2009
-- License:     GPL 2.0
-- Stability:   experimental
-- Portability: portable
--
-- Derive 'Autolib.ToDoc.Class.ToDoc' instances. Example:
--
-- @
-- $(derive makeReader ''Type)
-- @
--
-- Features:
--
--  * fields ending \"_info\" are omitted
--
-- Known bugs:
--
--  * infix type constructors are not handled correctly
--

module Data.Derive.ToDoc (
    -- * Derivations
    makeToDoc,
    -- * Reexports
    derive,
    derives
) where

import Data.DeriveTH    (derive, derives)
import Data.Maybe       (catMaybes)
import Data.List        (isSuffixOf)
import Control.Monad    (guard)
import qualified Language.Haskell as H
import Language.Haskell (
    Exp, CtorDecl, Decl, FullDataDecl,
    var, strE, intE, qvop, apps, (~=),
    ctorDeclName, ctorDeclFields, dataDeclCtors)

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

-- ^ 'Derivation' for 'ToDoc'

custom :: FullDataDecl -> [Decl] -> [Decl]
custom = customSplice splice

splice :: FullDataDecl -> Exp -> Exp
splice d (H.App x (H.Lit (H.Int y))) | x ~= "toDoc" = let
    fields = any (not . null . fst) (ctorDeclFields c)
    c = dataDeclCtors (snd d) !! fromInteger y
  in
    if fields then customRecord c else customData c
splice _ e = error $ "makeToDoc: unrecognized splice: " ++ show e

customData :: CtorDecl -> Exp
customData c | null (ctorDeclFields c) = cName c
             | otherwise = apps (var "docParen") [
    H.InfixApp (var "p") (qvop ">=") (intE 10),
    H.InfixApp (cName c) (qvop "</>") (
        var "fsep" `H.App` H.List (
            map (\i -> mkToDocPrec 10 (var ("x" ++ show i)))
                [1..length (ctorDeclFields c)]))]

customRecord :: CtorDecl -> Exp
customRecord c = apps (var "docParen") [
    H.InfixApp (var "p") (qvop ">=") (intE 10),
    H.InfixApp (cName c) (qvop "</>") (
        var "dutch_record" `H.App` H.List (
            catMaybes (zipWith singleField [1..] (ctorDeclFields c))))]
  where
    singleField nr (fn, _) = do
         guard $ not $ "_info" `isSuffixOf` fn
         return $ foldr1 (flip H.InfixApp (qvop "<+>")) $
             [mkText (strE fn),
              var "equals",
              mkToDocPrec 0 (var ("x" ++ show (nr :: Int)))]

cName :: CtorDecl -> Exp
cName c = mkText (strE (ctorDeclName c))

mkText :: Exp -> Exp
mkText = (var "text" `H.App`)

mkToDocPrec :: Integer -> Exp -> Exp
mkToDocPrec p a = var "toDocPrec" `H.App` intE p `H.App` a
