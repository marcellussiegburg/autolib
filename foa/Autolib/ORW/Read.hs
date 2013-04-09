{-# language PatternSignatures #-}

module Autolib.ORW.Read where

import Autolib.ORW.Data
import qualified Autolib.Exp as E

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Symbol
import Autolib.Reporter hiding ( join )

instance Symbol c => Reader (ORW c) where
    reader = do
        x <- reader
        case export $ exp2orw x of
            ( Just o , _ ) -> return o
            ( Nothing, msg :: Doc ) -> 
                error $ show msg

exp2orw :: Symbol c => E.RX c -> Reporter (ORW c)
exp2orw x = case x of
    E.Dot p q -> liftM2 join (exp2fin p) ( exp2orw q)
    E.PowerOmega p -> do
        p <- exp2fin p
        return $ ORW { prefix = [], period = p }
    _ -> reject $ text "Teilausdruck nicht erlaubt:"
                <+> toDoc x

exp2fin :: Symbol c => E.RX c -> Reporter [c]
exp2fin x = case x of
    E.Dot p q -> liftM2 (++) (exp2fin p) ( exp2fin q)
    E.Letter c -> return [c]
    _ -> reject $ text "Teilausdruck nicht erlaubt:"
                <+> toDoc x

join :: [c] -> ORW c -> ORW c
join f o = o { prefix = f ++ prefix o }
