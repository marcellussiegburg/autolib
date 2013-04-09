module Autolib.LTL.Eval.ORW where

import Autolib.ORW.Type as O
import Autolib.LTL.Type
import Autolib.LTL.Assign as Assign

import Autolib.Reporter
import Autolib.ToDoc
import Control.Monad.State

import qualified Data.Map as M

import Prelude hiding ( until )

evaluate :: Formula -> ORW Assign -> Reporter Bool
evaluate f o = do
    (a, s) <- runStateT ( eval (f, o) ) M.empty 
    inform $ text "cache" <+> toDoc s
    return a

eval :: (Formula , ORW Assign )
     -> StateT (M.Map (Formula, ORW Assign) Bool)
               Reporter Bool 
eval = cached $ \ (f, o) -> case f of
    Variable v -> case Assign.lookup v (O.head o) of
        Just res -> return res
        Nothing  -> lift $ reject $ text "variable" <+> toDoc v <+> text "fehlt in Belegung" <+> toDoc (O.head o)
    Nullary nop -> case nop of
        Constant c -> return c
        _ -> whine f
    Unary uop g -> case uop of
        Not -> bool1 not (g, o) 
        Next -> eval (g, O.tail o)
        Always -> always g (O.tails o)
        Eventually -> eventually g (O.tails o)
        _ -> whine f
    Binary bop g h -> case bop of
        And -> 
          ite (eval (g,o)) (eval (h,o)) (return False)
        Or  -> 
          ite (eval (g,o)) (return True) (eval (h,o)) 
        Implies -> 
          ite (eval (g,o)) (eval (h,o)) (return True)
        Iff     -> bool2 (==) (g,o) (h,o) 
        Until   -> until g h (O.tails o)
        _ -> whine f
    _ -> whine f 

whine f = lift $ reject $ text "Autolib.LTL.Eval.ORW.eval: missing case for" <+> toDoc f

-- | execute y or n, used for modelling lazy evaluation
ite p y n = do
    x <- p
    if x then y else n

always g os = case os of
    [] -> return True
    o : os -> 
       ite (eval (g,o)) (always g os) (return False)

eventually g os = case os of
    [] -> return False
    o : os -> 
       ite (eval (g,o)) (return True) (always g os) 

until g h os = case os of
    [] -> return False
    o : os -> 
          ite (eval (h,o)) (return True)
        $ ite (eval (g,o)) (until g h os) 
                           (return False)

bool1 f x  = do
    liftM f (eval x)

bool2 f x y = do
    liftM2 f (eval x) (eval y)

cached :: (Monad m, Ord arg)
       => ( arg -> StateT (M.Map arg res) m res )
       -> ( arg -> StateT (M.Map arg res) m res )
cached f = \ arg -> do
    c <- get
    case M.lookup arg c of
        Nothing -> do
            res <- f arg
            modify $ M.insert arg res 
            return res
        Just res -> do
            return res
