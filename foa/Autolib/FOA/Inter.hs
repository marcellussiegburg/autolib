-- | from omega-regexp to automaton

{-# language FlexibleContexts #-}

module Autolib.FOA.Inter where

import Autolib.FOA.Data
import Autolib.FOA.Op

import Autolib.Exp
import qualified Autolib.Exp.Inter as I
import qualified Autolib.NFA as N

import Autolib.ToDoc
import Autolib.Reporter

import qualified Data.Map as M

type Env c = 
     M.Map String (Either (N.NFA c Int) (FOA c Int))

inter :: (N.NFAC c Int, FOAC c Int )
      => Env c -> RX c 
      -> Reporter (FOA c Int)
inter env x = case x of
    Ref name -> case M.lookup name env of
        Just v -> case v of
            Right a -> return a
            Left {} -> whine_no_omega x
        Nothing -> reject 
           $ text "Name nicht gebunden:" <+> toDoc x 
    Letter c -> whine_no_omega x
    Dot l r -> do
        il <- inter_finite env l
        ir <- inter env r
        return $ normalize $ times il ir
    Union l r -> do
        il <- inter env l
        ir <- inter env r
        return $ normalize $ union il ir
    PowerOmega y -> do
        iy <- inter_finite env y        
        when ( accepts_epsilon iy ) $ reject $ vcat 
            [ text "das Argument von ^w darf nicht das leere Wort enthalten"
            ]
        return $ power_omega iy
    _ -> reject $ vcat
        [ text "Operator ist für omega-reguläre Ausdrücke nicht definiert:"
        , nest  4 $ toDoc x
        ]

inter_finite env x = do
    let iso x = case x of 
            PowerStar {} -> True ; _ -> False
        wrong = filter iso $ subtrees x
    when ( not $ null wrong ) $ reject $ vcat
        [ text "Ausdruck soll eine Sprache von endlichen Wörtern bezeichnen, enthält aber Omega:"
        , nest 4 $ toDoc x
        ]
    let env' = M.fromList $ do 
            (k, Left v) <- M.toList env
            return (k, v)
    return $ I.inter env' x

whine_no_omega x = reject $ vcat 
    [ text "Teilausdruck bezeichnet Automaten für endliche Wörter anstatt für Omega-Wörter:" <+> toDoc x ]