-- | see http://www.cse.unsw.edu.au/~cs4151/lecture7a.pdf

module Autolib.LTL.LocalEventual where

import Autolib.LTL.Type
import Autolib.FOA.Data
import Autolib.FOA.Op
import Autolib.FOA.Prop

import qualified Autolib.NFA as N
import qualified Autolib.NFA.Basic as N
import Autolib.Symbol 

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Autolib.Hash

import qualified Data.Map as M
import qualified Data.Set as S

import Prelude hiding ( not, and, or, until )
import qualified Prelude
import Data.List ( inits, tails )

and = Binary And ; or = Binary Or
implies = Binary Implies ; until = Binary Until
next = Unary Next ; eventually = Unary Eventually

-- | avoid double negation
not f = case f of
    Unary Not g -> g
    _ -> Unary Not f

reduce :: Formula -> Formula
reduce f = case f of
    Binary Or g h -> 
        not $ and (not $ reduce g) (not $ reduce h)
    Binary Implies g h -> 
        reduce $ or (not g) h
    Binary bop g h -> 
        Binary bop (reduce g) (reduce h)
    Unary Always g ->
        reduce $ not $ eventually $ not g
    Unary Eventually g ->
        reduce $ until (Nullary (Constant True)) g
    Unary Not g -> not $ reduce g
    Unary Next g -> next $ reduce g
    Nullary (Constant False) -> 
        not $ Nullary (Constant True)
    _ -> f

-- | set of subformulas and negated subformulas    
closure :: Formula -> S.Set Formula
closure f = S.fromList $ do
    g <- subtrees $ reduce f
    [ g, not g ]

-- | set of maximal subsets that have no
-- propositional inconsistency.
-- argument is a closure.
-- NOTE: for f = eventually always p,
-- sub (closure f) has 16 elements.
-- this implies that the alphabet has size 2^16 (!)
sub :: S.Set Formula -> S.Set ( S.Set Formula )
sub cls = maximal
        $ S.filter ( \ s -> consistent s && closed s )
        $ subsets cls

maximal s = S.fromList $ do
    (x, rest) <- splits s
    guard $ Prelude.not $ Prelude.or $ do
        y <- S.toList rest
        return $ S.isSubsetOf x y
    return x    

splits s = do
    let xs = S.toList s
    ( pre, this : post ) <- zip (inits xs) (tails xs)
    return (this, S.fromList $ pre ++ post )

consistent s = Prelude.and $ do
    (Unary Not x , rest ) <- splits s
    y <- S.toList rest
    return $ x /= y

closed s = Prelude.and $ do
    (Binary And x y, rest ) <- splits s
    return $ S.member x rest && S.member y rest

subsets :: Ord a => S.Set a -> S.Set (S.Set a)
subsets s = S.fromList $ do
    let xs = S.toList s
    ys <- sequence $ replicate (length xs) [False,True]
    return $ S.fromList
           $ map fst $ filter snd $ zip xs ys


